now := $(shell date +%Y%m%d%H%M)

cibuild: cabalinstall deb

citest:
	echo "No tests"

cabalinstall:
	cabal sandbox init
	cabal install

deb:
	rm -rf install_root
	mkdir -p install_root/usr/sbin
	mkdir -p install_root/etc/debcd/tests.d
	mkdir -p install_root/etc/cron.daily
	mkdir -p install_root/etc/cron.hourly

	cp .cabal-sandbox/bin/debcd install_root/usr/sbin/
	cp pass install_root/etc/debcd/tests.d/
	cp debconf.yml.example install_root/etc/debcd/

	cp debcd.nightly install_root/etc/cron.daily/
	cp debcd.hourly install_root/etc/cron.hourly/
	cd install_root && fpm -s dir -t deb -n debcd -v 0.1.$(now) -d aptitude --prefix / .
	cd /srv/reprepro/ubuntu && reprepro includedeb openbrain /tmp/debcd/install_root/debcd_0.1.$(now)_amd64.deb

