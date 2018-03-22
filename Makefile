core-win: core.r
	iconv -f utf8 -t cp936 core.r -o core-win.r
clean:
	rm core-win.r
