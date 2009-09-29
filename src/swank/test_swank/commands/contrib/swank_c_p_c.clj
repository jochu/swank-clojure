(ns swank.test-swank.commands.contrib.swank-c-p-c
  (:use swank.commands.contrib.swank-c-p-c
        swank.commands.contrib.swank-c-p-c.internal
        clojure.test))

(deftest delimited-compound-prefix-matches
  (testing "matches"
    (are [delimiter prefix target]
         (delimited-compound-prefix-match? delimiter prefix target)
         "."  "o.t.t"    "one.two.three"
         "-"  "on-tw"    "one-two-three"
         ".-" "on-t.thr" "one.two.three"))
  (testing "mismatches"
    (are [delimiter prefix target]
         (not (delimited-compound-prefix-match? delimiter prefix target))
         "."  "o-t.t" "one-two.three"
         "_"  "o_t_t" "one_two_four"
         "."  "o..t"  "one.two")))

(deftest delimited-compound-prefix-matches-acronyms
  (testing "matches with acronyms"
    (are [delimiter prefix target]
         (delimited-compound-prefix-match-acronym? delimiter prefix target)
         "."  "ott" "one.two.three"
         ".-" "ott" "one-two.three"))

  (testing "mismatches with acronyms"
    (are [delimiter prefix target]
         (not (delimited-compound-prefix-match-acronym? delimiter prefix target))
         "."  "ott" "one.two-three"
         ".-" "ott" "one-two.four")))

(deftest camel-compound-prefix-matches
  (testing "matches"
    (are [prefix target] (camel-compound-prefix-match? prefix target)
         "tSS"   "toSimpleString"
         ".S"    ".toString"
         ".tStr" ".toString"))

  (testing "mismatches"
    (are [prefix target] (not (camel-compound-prefix-match? prefix target))
         "tSS"   ".toSimpleString"
         ".S"    "toString")))

(deftest split-compound-prefix-matches
  (testing "matches"
    (are [prefix target] (split-compound-prefix-match? prefix target)
         "one/two"      "one/two-three"
         "three.f/five" "three.fix/five"
         "nst/jat"      "name.space.test/just-another-test"))

  (testing "mismatches"
    (are [prefix target] (not (split-compound-prefix-match? prefix target))
         "o.t"      "one/two-three"
         "imatch"   "i.do.not.match")))
