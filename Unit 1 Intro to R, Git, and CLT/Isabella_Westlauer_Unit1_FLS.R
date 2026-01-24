---
title: "Unit 1 R Markdown For Live Session"
author: "Isabella"
date: "1/21/2026"
output:
  html_document: default
  word_document: default
---
  
# Unit 1 Data Scientist Profile

skill = c(
  "Data Viz",
  "Machine Learning",
  "Mathematics",
  "Statistics",
  "Computer Science",
  "Communication",
  "Domain Expertise"
)
level = c(3,1,7,6,5,7,4)

barplot(
  level,
  names.arg = skill,
  col = "blue",
  ylim = c(0, 8),
  main = "Data Scientist Profile",
  las = 2 #makes the labels vertical
)

