(ns rsa.prime-generator-test
  (:require [clojure.test :refer :all]
            [rsa.prime-generator :as pg]))

(deftest is-prime
  (testing "Is prime Miller-Rabin"
    (is (true? (pg/mr-primality-test (BigInteger. "13"))))
    (is (true? (pg/mr-primality-test (BigInteger. "61"))))
    (is (true? (pg/mr-primality-test (BigInteger. "178542003245811211274167228297361192303886321036074276889145691522634525820185614278499562592134188995169731066418203258297035264969457638591284906658912408319763156912951486020761069099132619194489006875108217247513715271974383296142805846405783845170862140174184507256128825312324419293575432423822703857091")))))
  (testing "Is not prime Miller-rabin"
    (is (false? (pg/mr-primality-test (BigInteger. "12"))))
    (is (false? (pg/mr-primality-test (BigInteger. "12345"))))
    (is (false? (pg/mr-primality-test (BigInteger. "7839247328942732"))))
    (is (false? (pg/mr-primality-test (BigInteger. "4")))))
  )

(is-prime)