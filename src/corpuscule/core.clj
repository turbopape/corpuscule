(ns corpuscule.core
  (:require [instaparse.core :as insta]
            [clojure.xml :refer [parse]]
            [clojure.core.match :refer [match]]
            [stemmer.snowball :as snowball]
            [clojure.zip :as z]))


(def seq-grammar
  "S = <OSPC><LP> DSEXP <RP><OSPC>
   DSEXP =<OSPC><LP> <OSPC> ATOM (<OSPC> DSEXP | <SPC> ATOM)+ <OSPC> <RP> <OSPC>
   LP = '('
   RP = ')'
   ATOM = #\"(?iU)[\\w\\.\\-_\\\"\\?',+;%:!\\/°#=©<>½±\\[\\]&^$]+\"
   SPC = #'\\s+'
   OSPC = #'\\s*'")

(def parser (insta/parser seq-grammar :string-ci true))

(defn transf-seq
  [tree]
  (insta/transform
   {:S (fn [& toks] (into [] toks))
    :DET identity
    :DSEXP (fn [& toks ] (into [] toks))
    :ATOM identity}
   tree))

(def parse-seq
  (comp transf-seq parser))

 #_"( (SENT (PP-MOD (P En) (NP (NC 1953))) (PONCT ,) (NP-SUJ (DET les) (ADJ hauts) (NC fourneaux) (COORD (CC et) (NP (NC fonderies) (PP (P de) (NP (NPP Cousances)))))) (VN (V virent)) (NP-OBJ (DET le) (NC jour)) (PONCT ,) (COORD (CC puis) (Sint (NP-SUJ (NPP Jean) (NPP Baudesson) (PONCT ,) (NP-MOD (NC maire) (NC échevin) (PP (P de) (NP (NPP Saint-Dizier)))) (PONCT ,) (VPpart (VPP autorisé) (PP-MOD (P par) (NP (NC lettres) (AP (ADJ patentes)) (PP (P d') (NP (NPP Henri) (AP (ADJ IV)))))))) (PONCT ,) (VN (V installa)) (PP-P_OBJ (P à) (NP (NPP Marnaval) (PONCT -) (Srel (NP-SUJ (PROREL qui)) (VN (V signifiait)) (NP-OBJ (NC val) (COORD (CC ou) (NP (NC vallée))) (PP (P de) (NP (DET la) (NPP Marne))) (COORD (CC ou_bien) (PP (P en) (NP (NC aval) (PP (P de) (NP (DET la) (NPP Marne)))))))) (PONCT -))) (PONCT ,) (NP-OBJ (DET une) (NC forge) (Srel (NP-SUJ (PROREL qui)) (VN (V connut)) (NP-OBJ (DET son) (NC apogée)) (PP-MOD (P+D au) (NP (ADJ XIXe) (NC siècle))))))) (PONCT .)))"
(defn gen-annotated-seq-sentence
  [sent]
  (let [stemmer (snowball/stemmer :french)
        ast (parse-seq sent)
        vzpr (z/vector-zip ast)]
    (loop [loc (-> vzpr z/down)
           result []]
      (if (-> loc z/end?)
        result
        (recur (-> loc z/next)
               (let [node (-> loc z/node)]
                 (match [node]
                        [[(d :guard (comp not vector?))
                          (n :guard (comp not vector?))]] (conj result [(-> n
                                                                            clojure.string/lower-case) d])
                        :else result)))))))

#_"Par_P sa_DET sincérité_NC ,_PONCT sa_DET simplicité_NC ,_PONCT il_CLS savait_V toucher_VINF nos_DET cœurs_NC ,_PONCT dans_P les_DET rires_NC comme_P dans_P les_DET larmes_NC ._PONCT"

#_"Pour_P visionner_VINF ce_DET discours_NC en_P basse_ADJ définition_NC ,_PONCT [_PONCT http://upload.wikimedia.org/wikipedia/commons/thumb/0/07/President_Obama_Makes_a_Statement_on_the_Shooting_in_Newtown_FR_.ogv/320px--President_Obama_Makes_a_Statement_on_the_Shooting_in_Newtown_FR_.ogv.jpg_NC cliquez_V -_PONCT ici_ADV ]_PONCT ._PONCT"

(defn gen-annotated-tb-sentence
  [sent]
  (let [words (clojure.string/split sent #"\s+" )]
    (into [] (as-> words w
               (mapv #(let [toks (clojure.string/split % #"_")]
                        [ (reduce str (butlast toks)) (last toks) ]) w )
               (map (fn [[t d]] [(clojure.string/lower-case t) d]) w)))))



(defn gen-postagga-corpora
  [fn-parse-annotations resource] ;; parse annotations  : from text -> [["word" V]...] 
   (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource resource))]
    (loop [sents (line-seq rdr)
           result []]
      (if (seq sents)
        (recur
         (rest sents)
         (conj result (fn-parse-annotations (first  sents))))
        result))))



  
;; fndata-1.7/fulltext/WikiTexts__Fires_1.xml

(defn get-tag
  [xml-tree tag]
  (->>  xml-tree 
        (filter #(= (:tag %) tag))))

(defn get-attr
  [entries attr]
  (as-> entries e
    (filter #(= (get-in % [:attrs :name])
                attr) e)))

(defn parse-sentence-annotation
  [annotation sent]
  {:text  (as->  sent p
            (:content p)
            (get-tag p  :text)
            (first p)
            (:content p)
            (first p))
   :annotations (as-> sent p
                      (:content p)
                      (get-tag p :annotationSet)
                      (map :content p) (mapcat #(get-attr % annotation ) p)
                      (filter (comp not empty?) p)
                      (map :content p)
                      (first p)
                      (get-tag p :label)
                      (map :attrs p))})

(def parse-sentence-for-pos
  (partial parse-sentence-annotation "PENN"))

(defn emit-pos-tags
  [sent]
  (let [{:keys [text annotations]} (parse-sentence-for-pos sent)]
    (loop [rem-annotations annotations
           result []]
      (if (seq rem-annotations)
        (recur (rest rem-annotations)
               (conj result  (let [{:keys [start end name] :as annotation} (first rem-annotations)]
                               [(clojure.string/lower-case (subs text
                                                                 (Integer/parseInt start)
                                                                 (inc (Integer/parseInt  end))))
                                (clojure.string/upper-case name)])))
        result))))

(defn parse-fn-file
  [file]
  (let [;xml-file (clojure.java.io/as-file (clojure.java.io/resource resource))
        
        xml-tree (xml-seq (parse file))
        sentences (get-tag xml-tree :sentence)
        annotated-sents (map emit-pos-tags sentences)]
    (loop [rem-annos annotated-sents
           result []]
      (if (seq rem-annos)
        (recur (rest rem-annos)
               (conj result (first  rem-annos)))
        result))))

(defn parse-fn-resources
  [folder-resource]
  (let [folder (clojure.java.io/as-file (clojure.java.io/resource folder-resource))]
    (loop [rem-files (file-seq folder)
           result []]
      (if (seq rem-files)
        (recur (rest rem-files)
               (let [pos-tags (try  (parse-fn-file (first rem-files))
                                    (catch Exception e []))]
                 (if (not (empty? pos-tags))
                   (into result pos-tags)
                   result)))
        result))))


(def gen-postagga-corpora-from-sequoaia (partial gen-postagga-corpora
                                                   gen-annotated-seq-sentence))

(def gen-postagga-corpora-from-tb (partial gen-postagga-corpora
                                           gen-annotated-tb-sentence ))
#_(def tb-corpus
    (gen-postagga-corpora-from-tb  "frwikinews-20130110-pages-articles.txt.tok.stanford-pos"))
