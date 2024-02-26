Update with new versions of NVIdb on GitHub
================

  - [Overview](#overview)
  - [Make branch develop](#make-branch-develop)
  - [Publish a prerelease](#publish-a-prerelease)
  - [Publish a main release](#publish-a-main-release)

# Overview
The are two main branches, master and develop. 


Gammel Versjon
# Make branch develop
Create branch develop at \GitHub.com\NorwegianVeterinaryInstitute\NVIdb\

Create branch and copy develop from upstream to main
This is done in git menu in Rstudio (click on branch icon)

Create branch at \GitHub.com\PetterHopp\NVIdb\

pull from upstream to develop
git pull upstream -set develop
git pull upstream develop

push changes to personal GitHub account
git push origin --set develop
git push origin develop


make version number for development in DESCRIPTION, i.e. ending in .9000 for example v0.3.0.9000.

push changes to personal GitHub account

Merge changes from PetterHopp/NVIdb/develop to NorwegianVeterinaryInstitute/NVIdb/develop

pull from upstream to local/develop
git pull upstream develop

Update PetterHopp/NVIdb/develop
push changes (HEAD) to origin 
git push origin develop


FRA Oppdatering av NVIdb
13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (dev)
$ git pull upstream dev
remote: Enumerating objects: 2, done.
remote: Counting objects: 100% (2/2), done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 2 (delta 0), reused 0 (delta 0), pack-reused 0
Unpacking objects: 100% (2/2), 1.78 KiB | 165.00 KiB/s, done.
From https://github.com/NorwegianVeterinaryInstitute/NVIdb
 * branch            dev        -> FETCH_HEAD
 * [new branch]      dev        -> upstream/dev
Updating 8edf1d9..efe8b55
Fast-forward

13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (dev)

13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (main)
$ git branch -D dev
Deleted branch dev (was 8587046).


$ git checkout -b dev

$ git branch --set-upstream-to=origin/dev dev
Branch 'dev' set up to track remote branch 'dev' from 'origin'.

$ git pull upstream main
$ git pull upstream dev
From https://github.com/NorwegianVeterinaryInstitute/NVIdb
 * branch            dev        -> FETCH_HEAD
Already up to date.

13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (main)

13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (main)

13HOPP@VIA-L-5382KG3 MINGW64 ~/Documents/GitHub/NVIdb (dev)
$  git pull upstream dev

