(ns mytest
  (:use 
   [clojure.contrib.test-is]
   [compojure]))

(def session (ref {}))

(def data
     {:articles [{:description "the description", :body "the body"}]})

(deftest server-test 
  (is (= 1 (- 5 5))))

(defn login-view []
  (html
   [:body
    [:form {:method "post"}
     "User name: "
     [:input {:name "name", :type "text"}]
     [:br]
     "Password: "
     [:input {:name "password", :type "password"}]
     [:br]
     [:input {:type "submit" :value "Login"}]]]))

(defn login-controller [params]
  (dosync
   (if (and (= "secret" (params :password))
	    (.matches (params :name) "[\\w\\s\\-]+"))
     (do
       (alter session assoc :name (params :name))
       (redirect-to "/articles/"))
     (redirect-to "/login/"))))

(defn logout-controller []
  (dosync
   (alter session assoc :name nil)
   (redirect-to "/articles/")))

(defn page [title body]
  (html
   [:html
    [:head [:title title]]
    [:body
     [:h1 title]
     body
     [:p
      (if (@session :name)
	(link-to "/logout/"
		 (str "Log out " (@session :name)))
	(link-to "/login/" "Log in"))]]]))

(defn render-article-link [article]
  [:div [:p [:em (article :description)]]
   (article :body)])

(defn fetch-articles []
  (data :articles))

(defn view-article-list []
  (page "Articles"
	[:dl (mapcat
	      (fn [article]
		(list [:dt (render-article-link article)]
		      [:dd (article :description)]))
	      (fetch-articles))]))

(defn go-home []
  (redirect-to "/articles/"))

(defn ensure-admin-controller []
  (dosync
   (if (and (@session :name) (= (@session :name) "admin"))
     :next
     (go-home))))

(defn admin-view []
  (page "Admin" [:p "Only admin can see this page."]))

(defroutes webservice
  (ANY #"/articles(/*)" (view-article-list))
  (GET #"/login(/*)" (login-view))
  (POST #"/login(/*)" (login-controller params))
  (ANY #"/logout(/*)" (logout-controller))
  (ANY #"/admin(/*)" (ensure-admin-controller)) ; verification step
  (ANY #"/admin(/*)" (admin-view)))

(def server (run-server {:port 8080} "/*" (servlet webservice)))
(start server)


