(ns corpuscule.core
  (:require [instaparse.core :as insta]
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


(def gen-postagga-corpora-from-sequoaia (partial gen-postagga-corpora
                                                 gen-annotated-seq-sentence))

(def gen-postagga-corpora-from-tb (partial gen-postagga-corpora
                                           gen-annotated-tb-sentence ))
#_(def tb-corpus
    (gen-postagga-corpora-from-tb  "frwikinews-20130110-pages-articles.txt.tok.stanford-pos"))
