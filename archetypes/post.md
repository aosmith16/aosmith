---
title: ""
author: ""
date: {{ .Date }}
slug: []
categories:
- category
- subcategory
tags:
- tag1
- tag2
draft: yes
description: "Enter description"
---

```{r setup, include = FALSE, message = FALSE, purl = FALSE}
knitr::opts_chunk$set(comment = "#")
devtools::source_gist("2500a85297b742c6f2fb3a14549f5851",
                      filename = 'render_toc.R')
```

## Table of Contents

```{r toc, echo = FALSE, purl = FALSE} 
render_toc("2019-06-24-how-to-make-a-function-for-model-fitting.Rmd")
```

# Just the code, please

```{r getlabels, echo = FALSE, purl = FALSE}
labs = knitr::all_labels()
labs = labs[!labs %in% c("setup", "toc", "getlabels", "allcode", "makescript")]
```

```{r makescript, include = FALSE, purl = FALSE}
options(knitr.duplicate.label = "allow") # Needed to purl like this
input  = knitr::current_input()  # filename of input document
output = here::here("static", "script", paste(tools::file_path_sans_ext(input), "R", sep = ".") )
knitr::purl(input, output, documentation = 0, quiet = TRUE)
```

Here's the code without all the discussion.  Copy and paste the code below or you can download an R script of uncommented code [from here](/script/2019-06-24-how-to-make-a-function-for-model-fitting.R).

```{r allcode, ref.label = labs, eval = FALSE, purl = FALSE}
```
