# Tasks

<!-- maid-tasks -->

## docs

```sh
mkdir -p docs/spec
pandoc -f rst+lhs --toc --toc-depth=2 --number-sections \
  --template=docs/_layout.html bootstrap/Main.lhs -o docs/spec/index.html
```
