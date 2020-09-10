(defproject metabase-driver-sybase "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [net.sourceforge.jtds/jtds "1.3.1"] ]
  :repl-options {:init-ns metabase-driver-sybase.core}

  :profiles {:provided
             {:dependencies
              [[metabase-core "1.0.0-SNAPSHOT"]]}}
  :uberjar
  {:auto-clean    true
   :aot           :all
   :javac-options ["-target" "1.8", "-source" "1.8"]
   :target-path   "target/%s"
   :uberjar-name  "sybase.metabase-driver.jar"})
