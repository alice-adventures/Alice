FIGURES = $(shell ls -1 --color=no img/*-all.png img/*-dark.png img/*-light.png | xargs -I{} basename "{}" .png | sed -e 's:-\(light\|dark\)$$::' | uniq)

image-tags:
	@MAX_IMAGE_WIDTH=4000; \
	MAX_WIDTH=0; \
	for d in $(FIGURES); do \
		if [ $${d%-all} != $$d ]; then \
			FILE=img/$${d}.png; \
		else \
			FILE=img/$${d}-dark.png; \
		fi; \
		WIDTH=$$(file $$FILE | cut -d' ' -f 5); \
		if [ $$WIDTH -lt $$MAX_IMAGE_WIDTH ]; then \
			if [ $$MAX_WIDTH -lt $$WIDTH ]; then \
				MAX_WIDTH=$$WIDTH; \
			fi; \
		fi; \
	done; \
	for d in $(FIGURES); do \
		if [ $${d%-all} != $$d ]; then \
			FILE=img/$${d}.png; \
		else \
			FILE=img/$${d}-dark.png; \
		fi; \
		WIDTH=$$(file $$FILE | cut -d' ' -f 5); \
		if [ $$WIDTH -lt $$MAX_IMAGE_WIDTH ]; then \
			BASE=$$(basename $$d .png); \
			PCT=$$(echo $$WIDTH/$$MAX_WIDTH*90 | bc -l); \
			if [ $${d%-all} != $$d ]; then \
				printf "![$$BASE]($$BASE.png){ witdh=\"%4.2f%%\" }\n" \
					$$PCT; \
			else \
				printf "![$$BASE]($$BASE-dark.png#only-dark){ witdh=\"%4.2f%%\" }\n" \
					$$PCT; \
				printf "![$$BASE]($$BASE-light.png#only-light){ witdh=\"%4.2f%%\" }\n" \
					$$PCT; \
			fi; \
			echo "<br>**Figure X** - *<CAPTION>*"; \
			echo "{ .figure }"; \
			echo; \
		fi; \
	done;
