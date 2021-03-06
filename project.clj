(defproject stack "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 ]
  :main ^:skip-aot stack.core
  :target-path "target/%s"

  :clean-targets ^{:protect false} ["target"]
  :source-paths ["dev/server"]

  :profiles {
             :dev {
                   :repl-options {
                                  :init-ns          user
                                  :port             7001
                                  }
                   :env          {:dev true}
                   :dependencies [[binaryage/devtools "0.5.2" :exclusions [environ]]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [org.clojure/tools.nrepl "0.2.12"]]}}
  )
