library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)

df = read.csv("./data/patent.csv")
info = read.csv("./data/info.csv")


