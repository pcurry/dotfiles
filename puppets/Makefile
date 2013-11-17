PUPPET = puppet
PUPPETARGS =

.PHONY: provision
provision:
	$(PUPPET) apply --modulepath "modules/" $(PUPPETARGS) manifests/site.pp
