Update with new versions of NVIdb on GitHub
================

  - [Overview](#overview)
  - [Make branch develop](#make-branch-develop)
  - [Publish a prerelease](#publish-a-prerelease)
  - [Publish a main release](#publish-a-main-release)

# Overview
The are two main branches, master and develop. 



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

