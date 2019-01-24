# panda
Emacs package that consumes Bamboo's REST API to do useful things.

Steps to setup:                                                                    
  1. Place tfsmacs.el in your load-path.  Or install from MELPA (coming soon)      
  2. Customize 'panda' to add the Bamboo URL or manually:                          
     (setq 'panda-base-url "https://bamboo.yourorg.com/rest/api/latest"))          
     Notice, no trailing /                                                            
  3. Optionally, customize or manually set panda-username if you don't want        
     to enter your user name on each session                                       
                                                                                   
  The first request to Bamboo will ask for user/pass and then cache them in        
  memory as long as Emacs is open.  The information on the projects and plans      
  is retrieved once and cached for the session.  Call panda-refresh-cache to       
  for a refresh.  Branches for each plan are cached as required.                   
  There are no default key bindings yet (a keymap is in the roadmap)               
  The commands supported in this version are:                                      
                                                                                   
  Interactive:                                                                     
                                                                                   
  - panda-queue-build: starts a new build, interactively requests a project,         
                       plan and branch                                               
  - panda-build-results: gets the last 7 (by default) builds for a particular        
                         branch.                                                     
                                                                                   
  Non interactive:                                                                 
  - panda-build-results-branch: if you know you branch key you can call this         
                                function from elisp to display the build status      
                                                                                   
  In the roadmap: create deploys from builds, check deploy status, queue deploy,   
                  provide a keymap, improve documentation                          
