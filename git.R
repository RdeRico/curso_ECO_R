install.packages('usethis')

library('usethis')

use_git_config(user.name = 'RdeRico',
               user.email = 'rafaricomilan96@gmail.com')

usethis::create_github_token()
gitcreds::gitcreds_set()
