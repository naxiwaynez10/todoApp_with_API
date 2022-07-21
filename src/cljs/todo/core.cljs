(ns todo.core
  (:require [ajax.core :refer [GET]]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]))


(defonce api (r/atom {:sort :id :dir true :index 0 :result 10}))

(defn update-state 
  []
  (swap! api assoc :current (nth (partition (:result @api)  (:result @api) nil (:data @api)) (:index @api))))

(defn get-api
 "Get the Todos from the API https://jsonplaceholder.typicode.com/todos/"
  []
  (GET "https://jsonplaceholder.typicode.com/todos/"
  {:handler (fn [res]
              (swap! api assoc :data res)
              (update-state))
   :response-format :json
   :keywords? true}))




(defn sort-api
  "Sort the data in the API by ASCENDING or DESCENDING order"
  []
  (if (:dir @api)
    (sort-by (:sort @api) > (:current @api))
    (sort-by (:sort @api) < (:current @api))))

(defn class
 "Arrow to show sort order"
  []
  (if (:dir @api) (str "fa-arrow-up") (str "fa-arrow-down")))

(defn active
  "Add active as a class to active th"
  [ref]
  (if (= (:sort @api) ref) (str "active") (str "")))

(defn swal
  "Triger SweetAlert from Javascript using a mapped arg"
  ([text] (if (map? text) (js/swal (clj->js text)) (js/swal text)))
  ;; ([title text & {:key [buttons] :as config}] (js/swal (clj->js (conj {:title title :text text} config))))
  )


(defn change-state  
 " Updates the state of the APP"
  "You can send an AJAX request here to update the backend"
  
  [{:keys [id title completed userid]} & title?]
  (let [curr  (remove #(= (:id %) id) (:data @api))
        input [:input {:value title}]]
    (if title?
      (.then
       (swal {:title "Edit Task" :text "Change the title of the current Task" :buttons true :content {:element "input" :attributes {:value title}}})
       #(swap! api assoc :data (conj curr {:userId userid :id id :title (if (not= % true) (str %) title) :completed completed}))
       (js/$.notify "Task title changed successfully" "success"))
      (do (swap! api assoc :data (conj curr {:userId userid :id id :title title :completed completed}))
          (if completed (js/$.notify "Task state changed to DONE successfully" "success") (js/$.notify "Task state changed to UNDONE" "success"))))))

(defn tr
 "Render the <tr> element for each todos"
  [item]
  (let [id (:id item)
                 userId (:userId item)
                 title (:title item)
                 completed (:completed item)]
             [:tr {:key id}
              [:th {:scope "row"} id]
              [:td userId]
              [:td (if completed [:strike  title] [:span {:on-double-click #(change-state {:userid userId :id id :title (-> % .-target .-innerText) :completed completed} true)} title])]
              [:td
               [:input
                {:type "checkbox"
                 :id id
                 :on-change #(change-state {:userid userId :id id :title title :completed (not completed)})
                 :checked completed}]]]))
  

(defn todos []
  (r/create-class
   {:component-will-mount (fn [_] (get-api))
    :component-did-update (fn [x _] (update-state))
    :reagent-render (fn []
                      [:div.card.mt-5
                       [:div.card-heading.text-center.p-5 [:h2 "List Tasks"]
                        [:div.form-group
                         [:div.row
                          [:div.col-10
                           [:div.form-group
                            [:label {:for "sort"} "Search Title"]
                            [:input.form-control {:value (:title @api)
                                                  :on-change #(swap! api assoc :title (-> % .-target .-value))
                                                  :placeholder "Search for title here.."}]]]
                          [:div.col-2
                           [:div.form-group
                            [:label {:for "select"} "Select Range of result"]
                            [:select.form-control
                             {:on-change #(swap! api assoc :result (-> % .-target .-value int))}
                             [:option {:value "10"} 10]
                             [:option {:value "20"} 20]
                             [:option {:value "50"} 50]
                             [:option {:value "100"} 100]
                             [:option {:value "200"} 200]]]]]]]
                       [:div.row
                        [:div.col-10.offset-1
                         [:nav {:aria-label "Page navigation example"}
                          [:ul {:class "pagination"}
                           [:li (if (= 0 (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
                            [:button {:class "page-link"
                                      :on-click #(swap! api assoc :index (- (:index @api) 1))} "Previous"]]
                           (map (fn [x]
                                  [:li {:class (if (= (:index @api) x) (str "page-item active") (str "page-item")) :key x}
                                   [:button {:class "page-link"
                                             :key x
                                             :on-click #(swap! api assoc :index x)} (+ 1 x)]]) (range (int (/ (count (:data @api)) (:result @api)))))

                           [:li (if (= (- (int (/ (count (:data @api)) (:result @api))) 1) (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
                            [:button {:class "page-link"
                                      :on-click #(swap! api assoc :index (+ (:index @api) 1))} "Next"]]]]]]

                       [:div.card-body
                        [:table {:class ".mt-5 table"}
                         [:caption "List of users"]
                         [:thead
                          [:tr
                           [:th {:scope "col"} "#"]
                           [:th {:scope "col"
                                 :class (active :userId)
                                 :on-click #(swap! api assoc :sort :userId :dir (not (:dir @api)))} "userId  " [:i.fa {:class (class)}]]
                           [:th {:scope "col"
                                 :class (active :title)
                                 :on-click #(swap! api assoc :sort :title :dir (not (:dir @api)))} "Title  " [:i.fa {:class (class)}]]
                           [:th {:scope "col"
                                 :class (active :completed)
                                 :on-click #(swap! api assoc :sort :completed :dir (not (:dir @api)))} "completed?  " [:i.fa {:class (class)}]]]]
                         [:tbody
                          (map (fn [item]
                                 (if-not (empty? (:title @api))
              ;;  There is a search keyword
                                   (if (str/includes? (str (:title item)) (str (:title @api))) (tr item))
                                   (tr item)))  (sort-api))]]]])}))


(defn ^:dev/after-load init! []
  (rdom/render [todos] (.getElementById js/document "app")))

;; (init!)