# panda
Emacs package that consumes Bamboo's REST API to do useful things.


## Installation

1. Place tfsmacs.el in your load-path.  Or install from MELPA (coming soon)
 
2. Customize 'panda' to add the Bamboo URL or manually _(Notice there's no
trailing /)_:
```elisp
    (setq 'panda-base-url "https://bamboo.yourorg.com/rest/api/latest"))          
```

3. Optionally, customize or manually set panda-username if you don't want to enter
your user name on each session

4. There's a keymay provided for convenience:
```elisp
    (require 'panda)
    (global-set-key (kbd "C-c b") 'panda-map) ;; b for "Bamboo"
```

The first request to Bamboo will ask for user/pass and then cache them in memory as
long as Emacs is open. The information on the projects and plans is retrieved once
and cached for the session. Branches for each plan are cached as required. Information
about the deployment projects is cached at once too. Call `panda-refresh-cache`to force a
reload of all items.

The commands supported in this version

Interactive:

* panda-queue-build: starts a new build, interactively requests a project,
                   plan and branch
* panda-build-results: gets the last 7 (by default) builds for a particular
                     branch.
* panda-queue-deploy: starts a new deploy, interactively requests a plan,
                    environment and deploy.
* panda-deploy-status: gets the status of all environments for a deploy
                     project.
* panda-clear-credentials: force inputting user/pass in the next API call.

* panda-refresh-cache: re-fetch list of build plans and deploy projects.

Non interactive:

* panda-build-results-branch: if you know you branch key you can call this
                            function from elisp to display the build status

## Roadmap

* Create deploys from builds
* Find a way to tell if a build has a deploy created
* Improve documentation
