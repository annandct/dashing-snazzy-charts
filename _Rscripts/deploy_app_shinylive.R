#deploy via shinylive()

#install.packages("shinylive")
#library(shinylive)
#?shinylive::export

shinylive::export(appdir =  getwd(),
                  destdir = "../_deployedApps/",
                  template_params = list(
                    title = "The CE Shop - Sample_Dashboard",
                    on_start = "Please wait while the app loads...")
                  )

#to test run the app locally
#httpuv::runStaticServer("../_deployedApps/")