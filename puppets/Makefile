PUPPET = puppet
PUPPETARGS =

.PHONY: all settings

all:
	$(PUPPET) apply --modulepath "modules/" $(PUPPETARGS) manifests/all.pp

settings:
	$(PUPPET) apply --modulepath "modules/" $(PUPPETARGS) -e 'include lunaryorn::user::settings'
