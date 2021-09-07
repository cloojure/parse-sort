(defproject demo-grp/demo-art "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [camel-snake-kebab "0.4.2"]
                 [clojure.java-time "0.3.3"]
                 [hiccup "1.0.5"]
                 [http-kit "2.5.3"]
                 [io.pedestal/pedestal.jetty "0.5.9"]
                 [io.pedestal/pedestal.route "0.5.9"]
                 [io.pedestal/pedestal.service "0.5.9"]
                 [io.tupelo/pedestal "20.02.03"]
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/test.check "1.1.0"]
                 [org.flatland/ordered "1.5.9"]
                 [org.slf4j/slf4j-simple "1.7.32"]
                 [prismatic/schema "1.1.12"]
                 [tupelo "21.09.03"]
                 ]
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
            [lein-ancient "0.7.0"]
            [lein-codox "0.10.7"]
            ]

  :profiles {:dev     {:dependencies []}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* false}

  :main demo.core
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :test-paths ["test/clj"]
  :target-path "target/%s"
  :compile-path "%s/class-files"
  :clean-targets [:target-path]

  :jvm-opts ["-Xms500m" "-Xmx4g"]
  )
