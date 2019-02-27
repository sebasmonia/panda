[![MELPA](https://melpa.org/packages/panda-badge.svg)](https://melpa.org/#/panda)

# panda
Emacs package that consumes Bamboo's REST API to do useful things.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
     * [Keymap](#keymap)
   * [Manual](#manual)
     * [Cache](#cache)
     * [Custom bindings](#custom-bindings)
     * [Releases](#releases)
   * [Roadmap](#roadmap)
<!--te-->

## Installation and configuration

Place panda.el in your load-path.  Or install from MELPA (preferred).

The next step would be to use customize for panda. Options are documented, below some of them:

1. **REQUIRED**: Add the Bamboo API URL _(Notice there's no trailing /)_:

```elisp
    (setq 'panda-api-url "https://bamboo.yourorg.com/rest/api/latest")
```

There's also `panda-browser-url`, which is used to open elements in your default browser.

2. You can set `panda-username` if you don't want to enter your user name on each session

3. Not all environments are created equal. Use `panda-deploy-confirmation-regex` to match
   environment names for which you want to be asked for confirmation before deploying.
   For example in my own config this is set to "prod".

4. `panda-less-messages` means the package won't display as many messages in the echo area.
   You can still read them all in the package's log buffer

5. The pair `panda-open-status-after-build` and `panda-open-status-after-deploy` default to 'ask.
   Their names are quite self-explanatory. I have them set to t so whenever I queue a build 
   or deploy, the status buffer for the relevant project opens up and I can check the progress.
   
6. Finally, `panda-log-responses` should help you (us!) troubleshoot any issues.

### Keymap

There's a keymay provided for convenience:
```elisp
    (require 'panda)
    (global-set-key (kbd "C-c b") 'panda-map) ;; b for "Bamboo"
```

The keymap use long sequences, but it's also quite mnemonic.

`C-c b s` (`s` => status), then either `b`, `d` or `e` for build, deploy or environment status, respectively.

`C-c b q` (`q` =>  queue), then either `b` or `d` to queue a build or a deploy.

Finally, the status buffers also have some actions associated to them. The echo area will
show the relevant keys. The most common one is `g` to refresh the data.

# Manual

In Bamboo, the builds follow a hierarchy of PROJECT -> PLAN -> optionally, BRANCHES.
The deployment projects use their own numeric ID. Using Panda itself, or checking out 
the Bamboo URL, is easy to identify these codes or descriptions to setup shortcut bindings
(more on this below).


For the rest of the manual we'll asume you work in a project called "Awesome" which has 
been configured in Bamboo as follows:

* Project code: AWE
  * Module Server: AWE-SERVER
  * Module Client: AWE-CLIENT

* Name of the deploy project: "Awesome - Server"

Whenever you call one of the main commands interactively, you need to tell Panda which
project you want to build or deploy. Using the cache, Panda will use `completing-read` to
map a project's name to their internal codes.

## Cache

The first request to Bamboo will ask for user/pass and then cache your credentials in memory
as long as Emacs is open. The information on the projects and plans is retrieved once
and cached for the session. Branches for each plan are cached as required. Information
about the deployment projects is cached at once too. 

If you created a new branch or environment in Bamboo and don't see them in panda, call 
`panda-refresh-cache` to force a reload of all items.

## Custom bindings

All the major commands accept the project's code or name, so you can bind your own
"most used" projects. For example:

```elisp
;; build commands  Project Awesome's server
(global-set-key (kbd "M-<prior>") (lambda () (interactive) (panda-queue-build "AWE-SERVER")))
(global-set-key (kbd "M-S-<prior>") (lambda () (interactive) (panda-build-results "AWE-SERVER")))
;; deploys for Project Awesome's server
(global-set-key (kbd "M-<next>") (lambda () (interactive) (panda-queue-deploy "Awesome - Server")))
(global-set-key (kbd "M-S-<next>") (lambda () (interactive) (panda-deploy-status "Awesome - Server")))
```

With custom bindings you can skip prompts and jump directly to the action, while keeping the main
commands flexible to deploy other projects when needed.

## Releases

Depending on your setup, you might need to manually create a releases out of your builds
before you deploy them:

* You can invoke `panda-create-release` or `C-c b c`, and follow the prompts.
* From the build status buffer, press `c` with a successful build under point to create a release.

If your plan does not create a release automatically, you will notice that after the build is done 
you can't see it in the list of possible releases. This should take care of that step.

## Roadmap

There are a few tasks I can't find a way to accomplish via the API:
* Telling if a build has a deploy created
* Finding the name of the base plan's branch from the data instead of hardcoding the name to "Base"
* A way to relate a build to a release via IDs, not depending on the release name matching the build's name.

Feel free to submit an issue for new features or bugs!

