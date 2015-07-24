{:user {:plugins [[cider/cider-nrepl "0.9.1"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.10"]]
        :signing {:gpg-key "junjiemars@gmail.com"
        :deploy-repositories [["clojars" {:creds :gpg}]]}
        }}

