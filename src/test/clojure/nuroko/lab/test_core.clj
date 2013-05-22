(ns nuroko.lab.test-core
  (:use nuroko.lab.core)
  (:use clojure.test))

(deftest test-neural-stack
  (testing "construction"
    (is (neural-network :inputs 3 :outputs 3))))

(deftest test-learn-factor
  (let [n (neural-network :inputs 3 :outputs 3)]
    (is (== 1.0 (learn-factor n)))
    (adjust-learn-factor! n 2.0)
    (is (== 2.0 (learn-factor n)))))

