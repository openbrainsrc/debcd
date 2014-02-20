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

	cp .cabal-sandbox/bin/debcd install_root/usr/sbin/
	cp pass install_root/etc/debcd/tests.d/
	cp debconf.yml.example install_root/etc/debcd/
	cd install_root && fpm -s dir -t deb -n debcd -v 0.1.`date +%Y%m%d%H%M` --prefix / .
