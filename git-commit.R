##### Load Packages ######
library(usethis)

##### One-time Setup (Run these once, then comment out) #####

## 1. Set Git credentials in credential manager (only need to run ONCE ever)
Sys.getenv('GITHUB_PAT_PERSONAL')
gitcreds::gitcreds_set()  # Paste your PAT when prompted

## 2. Set global git config (only need to run ONCE ever)
use_git_config(
  scope = "user",  # "user" = global config
  user.name = "ehsan-f",
  user.email = ""
)

##### Regular Workflow #####

## Commit and push changes
v_commit_message <- 'Updated package and functions'
gert::git_add(".")
gert::git_commit(v_commit_message)
gert::git_push()

##### Project Setup (For new projects) #####

## Initialize git and create GitHub repo
# use_git()
# use_github(private = TRUE)

##### Clone Existing Repo (For new machines) #####

## Clone a repository
# create_from_github(
#   repo_spec = "ehsan-f/mixtape",
#   destdir = "/home/ehsan/projects"
# )
