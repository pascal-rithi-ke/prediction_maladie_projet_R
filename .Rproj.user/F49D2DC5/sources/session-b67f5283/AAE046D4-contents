# Charger les packages nécessaires
source('lib.R')
source('import.R')

# Load variables from .env file
load_dot_env(file = ".env")

# SECRET_KEY_DEPLOY shinyApp.io
SECRET_KEY_DEPLOY <- Sys.getenv("SECRET_KEY_DEPLOY")

# Configurer les informations de compte shinyapps.io
rsconnect::setAccountInfo(name='3eqx4i-pascal-k0e',
                          token='2DEF610EDB6DDA7E5630B151D1B019B3',
                          secret=SECRET_KEY_DEPLOY)

# Définir le chemin vers le répertoire contenant l'application
appDir <- "."

# Déployer l'application
rsconnect::deployApp(appDir, account = '3eqx4i-pascal-k0e')