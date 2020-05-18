# Build and deploy to http://wwww.roger-beecham.com/predictive-analytics

SSH_USER = rogerbeecham@roger-beecham.com
DOCUMENT_ROOT = ~/roger-beecham.com/predictive-analytics/public_html
PUBLIC_DIR = public/
OUTPUTDIR = public_html
HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd))

.PHONY : all

all: deploy

clean:
	rm -rf public/
	find content -type f -name '*.html' -delete

build:
	Rscript -e "blogdown::build_site()"

serve: build
	Rscript -e "blogdown::serve_site()"

deploy: build
	rsync --exclude='.DS_Store' -Prvzce 'ssh -p 22' $(PUBLIC_DIR) $(SSH_USER):$(DOCUMENT_ROOT) --delete-after
