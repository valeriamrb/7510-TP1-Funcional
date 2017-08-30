(ns philosopher-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def philosopher-database "
	sinplumas(platon).
	sinplumas(perro).
	sinplumas(gallinadesplumada).
	bipedo(platon).
	bipedo(gallina).
	bipedo(gallinadesplumada).
	griego(platon).
	humano(X) :- bipedo(X), sinplumas(X).
	filosofo(X) :- humano(X), griego(X).
")

(deftest philosopher-database-rule-test
  (testing "humano(platon) should be true"
    (is (= (evaluate-query philosopher-database "humano(platon)")
           true)))
  (testing "humano(perro) should be false"
    (is (= (evaluate-query philosopher-database "humano(perro)")
           false)))
  (testing "humano(gallina) should be false"
    (is (= (evaluate-query philosopher-database "humano(gallina)")
           false)))
  (testing "humano(gallinadesplumada) should be true"
    (is (= (evaluate-query philosopher-database "humano(gallinadesplumada)")
           true))))

(deftest philosopher-database-chain-test
  (testing "filosofo(platon) should be true"
    (is (= (evaluate-query philosopher-database "filosofo(platon)")
           true)))
  (testing "filosofo(perro) should be false"
    (is (= (evaluate-query philosopher-database "filosofo(perro)")
           false)))
  (testing "filosofo(gallina) should be false"
    (is (= (evaluate-query philosopher-database "filosofo(gallina)")
           false)))
  (testing "filosofo(gallinadesplumada) should be false"
    (is (= (evaluate-query philosopher-database "filosofo(gallinadesplumada)")
           false))))
