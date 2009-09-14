(ns trends.controllers.users
  (:use 
   [trends.views.layout]
   [trends.security]
   [compojure]))

(defn- edit [user request]
  (page user "edit page"))

(defn users-context []
  (list
   (GET "/edit/:id" (logged-in-only edit request))))