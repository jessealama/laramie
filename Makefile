.PHONY: clean

clean:
	find . -type d -name compiled | xargs rm -Rf
	find . -type f -name '*~' -delete
