;; Runtime value capture for standard nREPL. Installed in the tooling session.
(binding [*ns* (create-ns 'xeon.repl.values)]
  (clojure.core/refer 'clojure.core)
  (require '[nrepl.transport :as transport]
           '[nrepl.misc :as misc]
           '[nrepl.middleware.interruptible-eval :as ie])
  (eval
   '(do
      (defonce objects (atom {}))
      (defonce counter (atom 0))
      (defn value [id]
        (if (contains? @objects id) (get @objects id)
            (throw (ex-info "This returned value has expired" {:id id}))))
      (defn evaluate [form]
        (let [result (clojure.core/eval form)
              id (swap! counter inc)]
          (swap! objects (fn [items] (let [items (assoc items id result)]
                                      (if (> (count items) 128)
                                        (dissoc items (apply min (keys items))) items))))
          (transport/send (:transport ie/*msg*)
                          (misc/response-for ie/*msg* :krypton-presentation (str id)))
          result)))))
