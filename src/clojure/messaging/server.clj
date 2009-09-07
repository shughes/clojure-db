(add-classpath "file:///Users/shughes/Desktop/clojure/messaging/lib/jetty-6.1.9.jar")
(add-classpath "file:///Users/shughes/Desktop/clojure/messaging/lib/jetty-util-6.1.9.jar")
(add-classpath "file:///Users/shughes/Desktop/clojure/messaging/lib/servlet-api-2.5-6.1.9.jar")

(ns clojure.messaging.server
  (:import 
   [java.io File BufferedReader InputStreamReader]
   [javax.servlet.http HttpServlet HttpServletRequest HttpServletResponse]
   [org.mortbay.jetty Server Handler Connector NCSARequestLog]
   [org.mortbay.jetty.handler HandlerCollection ContextHandlerCollection RequestLogHandler]
   [org.mortbay.jetty.nio BlockingChannelConnector]
   [org.mortbay.jetty.servlet Context DefaultServlet ServletHolder SessionHandler])
  (:require
   [clojure.contrib.json.write :as json-w]))

;; server port
(def *port* 8080)

;; static pages will be served from this directory
(def *wwwdir* "pages")

;; log files go here
(def *logdir* "log")

;; specifies naming pattern for log files
(def *logfiles* "log.yyyy_mm_dd.txt")

;; create connector on the specified port
(defn- make-connectors [port]
  (let [conn (new BlockingChannelConnector)]
    (. conn (setPort port))
    (into-array [conn])))


;; set up handlers: the context ones, and a logger to track all http requests
(defn- make-handlers [contexts]
  (let [handlers (new HandlerCollection)
        logger (new RequestLogHandler)
        logfile (new File *logdir* *logfiles*)]

    (. logger (setRequestLog (new NCSARequestLog (. logfile (getPath)))))
    (. handlers (addHandler logger))
    (. handlers (addHandler contexts))
    handlers))

;; make an empty collection of context handlers - we'll configure it later
(defn- make-contexts []
  (new ContextHandlerCollection))

;; Begin Test Servlets

(defn- make-rest-servlet []
  (proxy [HttpServlet] []
    (doGet [#^HttpServletRequest req #^HttpServletResponse resp]
	   (clojure.messaging.response/process-get req resp))
    (doPost [#^HttpServletRequest req #^HttpServletResponse resp]
	    (clojure.messaging.response/process-post req resp))))

;; End Test Servlets

;; configures a default servlet to serve static pages from the "/" directory
(defn- configure-context-handlers [contexts]
  (doto (new Context contexts "/" (. Context NO_SESSIONS))
    (.setWelcomeFiles (into-array ["index.html"]))
    (.setResourceBase (. (new File *wwwdir*) (getPath)))
    (.setSessionHandler (new SessionHandler))
    ;(.addServlet (new ServletHolder (new DefaultServlet)) "/*")
    (.addServlet (new ServletHolder (make-rest-servlet)) "/*")))

;; make an instance of the http server
(defn make-server
  ([] (make-server *port*))
  ([port]
     (let [contexts (make-contexts)
           connectors (make-connectors port)
           handlers (make-handlers contexts)
           server (new Server)]
       (. server (setConnectors connectors))
       (. server (setHandler handlers))
       (configure-context-handlers contexts)
       server)))
