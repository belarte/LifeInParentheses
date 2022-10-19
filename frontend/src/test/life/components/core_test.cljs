(ns life.components.core-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [reagent.core :as r]
            ["@testing-library/react" :as rtl]
            [life.components.core :as component]))

(use-fixtures :each {:after rtl/cleanup})

(defn with-mounted-component [component f]
  (let [mounted-component (rtl/render (r/as-element component))]
    (try
      (f mounted-component)
      (finally
        (.unmount mounted-component)
        (r/flush)))))

(deftest title-component
  (testing "A title is displayed"
    (with-mounted-component
      [component/title]
      (fn [component]
        (is (= "Life in parenthesis"
               (-> component
                   (.getByText #"(?i)life")
                   (.-innerHTML))))))))
