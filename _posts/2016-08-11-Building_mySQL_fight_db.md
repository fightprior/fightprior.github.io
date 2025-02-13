---
title: "Building a large database of MMA fight results IV: turning tables into a database with R and SQL"
description: "Creating and accessing a SQL database from R"
author: sean
layout: post
comments: true
tags: [database, dplyr, RMySQL]
---

Previously, I discussed acquiring and standardizing a compendium of fighter and fight-level MMA results.

- [I: Scraping fighter and fight data]({{ site.url }}/2016/04/29/scrapingMMA/): Aggregating data from ~250,000 MMA fights between ~140,000 fighters.
- [II: Cleaning up fight-level data]({{ site.url }}/2016/05/05/summarizingFights/): summarizing the major ways that fights are won.
- [III: Cleaning up fighter-level data]({{ site.url }}/2016/05/13/summarizingFighters/): summarizing where fighters are from and major weight classes.

In this post, I will go over how the previously created fight, fighter and event-level tables can be transformed into a MySQL database that can both be created and accessed using R.

<!--more-->

## Setting up MySQL

Before we can make an MMA database we may have to setup SQL. 

First we need to install a distribution of MySQL. For this purpose, I would recommend  [MySQLWorkbench](http://dev.mysql.com/downloads/workbench/). In addition to including a distribution of MySQL that can be accessed from the command line, MySQL Workbench also has a nice, interactive interface for making SQL queries and contains other features, such as tools to make an [Enhanced Entity-Relationship (EER) diagram](https://en.wikipedia.org/wiki/Entity%E2%80%93relationship_model).

Before we can actually start querying SQL, we need to start the local server. For OS X, I do this using the [MySQL Preference Pane](https://dev.mysql.com/doc/refman/5.1/en/osx-installation-prefpane.html), which allows you to start the server from Systems Preferences.

Another step that may be useful is creating an alias for MySQL so that it can be more easily worked with. I did this by adding the following code to my .bash_profile.


{% highlight bash %}
alias mysql=/usr/local/mysql/bin/mysql
alias mysqladmin=/usr/local/mysql/bin/mysqladmin
{% endhighlight %}

## Setting up the MMA database using MySQL

Once the initial setup of MySQL is complete, the next step is to create the database that we will populate with our fight data. 


{% highlight bash %}
mysql # login with root privileges
create database MMA;
grant all privileges on MMA.* to 'shackett'@'localhost' identified by 'password';
flush privileges;
{% endhighlight %}

## Populating the MMA database from R using RMySQL


{% highlight r %}
library(DBI)
library(RMySQL)
library(dplyr)

# Connect to the MySQL server
MMA_connection <- dbConnect(MySQL(), user='shackett', password='password', dbname='MMA', host='localhost')

# Load tables
bouts <- readRDS("~/Desktop/MMA/software/data/processed_fight_data/MMA_all_bouts.Rds")
fighters <- readRDS("~/Desktop/MMA/software/data/processed_fight_data/MMA_all_fighters.Rds")
events <- readRDS("~/Desktop/MMA/software/data/processed_fight_data/MMA_events.Rds")

# add to mySQL database
dbWriteTable(MMA_connection, "bouts", as.data.frame(bouts), row.names = F, overwrite = T)
dbWriteTable(MMA_connection, "fighters", as.data.frame(fighters), row.names = F, overwrite = T)
dbWriteTable(MMA_connection, "events", as.data.frame(events), row.names = F, overwrite = T)

dbDisconnect(MMA_connection)
{% endhighlight %}

## Working with our SQL database using MySQL Workbench

Now that we have piped the bouts, fighters and events into the MMA database we can start working with this dataset in SQL.

MySQL Workbench makes interacting with SQL queries very straightforward. For example, shown below, we can see all the results of bouts that ended in headbutts.

![SQL_query_ex]({{ site.url }}/figure/2016-08-11-Building_mySQL_fight_db/SQL_query_ex.png){: .align-center }

To better understand this dataset, it would be useful to know how fields in each dataset are connected. Generally, each table will have a primary key that uniquely identifies each entry (a row). For fighters and events, a logical choice for the table's primary key are the urls that pointed to the fighter and event's webpage (Query and Event_link, respectively). Individual bouts entries were derived from fighters' pages, so the table doesn't have any great choices for a primary key. To generate a unique primary key for bouts, we can add an additional field Fight_ID that provides a unique identifier to each bout entry. We can also auto-incriment this ID such that if additional fights are added, they will automatically be provided a new unique ID.

Primary keys are useful because they can be used to perform summaries at the fighter/bout/event level. An even more important role for these keys is that they tell us how to combine data. For example, we have both fighter-level data and bout-level data: if we want to be able to combine these two datasets, we need a clear way of linking fighter-level data to the fighter and his/her opponent in the bouts dataset. We can accurately carry out this combination because the bouts data contains two columns, Fighter_link and Opponent_link, that serve as foreign keys; their entries map to the entries of Query in the fighters table. A similar link is provided by the Event_link in the bouts data; it maps bouts to a specific MMA event stored in the events table.

An EER diagram for this dataset summarizes the fields in fighters, bouts and events as well as the primary key, foreign key relationships that link them:

![MMA_MySQL_schema]({{ site.url }}/figure/2016-08-11-Building_mySQL_fight_db/MMA_MySQL_schema.png){: .align-center }

## Querying SQL from R using dplyr

While SQL is a powerful language for querying and combining tables, if we want to analyze our database using R, it is useful to access data from within R.

One way that we can access our database from within R is by executing a SQL call from within R.





{% highlight r %}
MMA_connection <- dbConnect(MySQL(), user='shackett', password='password', dbname='MMA', host='localhost')

SQL_query <- "SELECT Fighter, Opponent, Finish, Event
	FROM bouts
	WHERE Finish = 'Headbutt'
  ORDER by Event;"

SQL_result <- dbGetQuery(MMA_connection, SQL_query)
dbDisconnect(MMA_connection)
{% endhighlight %}



{% highlight text %}
## [1] TRUE
{% endhighlight %}



{% highlight r %}
kable(head(SQL_result, 5), row.names = F)
{% endhighlight %}



|Fighter       |Opponent      |Finish   |Event                                                |
|:-------------|:-------------|:--------|:----------------------------------------------------|
|Ely Galvao    |Heleno Nunes  |Headbutt |BVF 11 - Circuito Brasileiro de Vale Tudo 4          |
|Heleno Nunes  |Ely Galvao    |Headbutt |BVF 11 - Circuito Brasileiro de Vale Tudo 4          |
|Roger Huerta  |Jeff Carlson  |Headbutt |EC - Best of the Best 2 - Day Event                  |
|Jeff Carlson  |Roger Huerta  |Headbutt |EC - Best of the Best 2 - Day Event                  |
|Sean Robinson |Chris du Pont |Headbutt |EFC Africa - Extreme Fighting Championship Africa 25 |

An alternative to making queries using SQL syntax is to use dplyr to set up similar select, filter, etc. commands and then fetch the data that we are interested in.


{% highlight r %}
library(dplyr)

# specify connection to database
SQL_result = src_mysql(user='shackett', password='password', dbname='MMA', host='localhost') %>%
  # point out which table we want
  tbl("bouts") %>%
  # Filtering, selecting and arranging
  filter(Finish == "Headbutt") %>%
  select(Fighter, Opponent, Finish, Event) %>%
  arrange(Event) %>%
  # execute query, collecting the data
  collect()

kable(head(SQL_result, 5), row.names = F)
{% endhighlight %}



|Fighter       |Opponent      |Finish   |Event                                                |
|:-------------|:-------------|:--------|:----------------------------------------------------|
|Ely Galvao    |Heleno Nunes  |Headbutt |BVF 11 - Circuito Brasileiro de Vale Tudo 4          |
|Heleno Nunes  |Ely Galvao    |Headbutt |BVF 11 - Circuito Brasileiro de Vale Tudo 4          |
|Roger Huerta  |Jeff Carlson  |Headbutt |EC - Best of the Best 2 - Day Event                  |
|Jeff Carlson  |Roger Huerta  |Headbutt |EC - Best of the Best 2 - Day Event                  |
|Sean Robinson |Chris du Pont |Headbutt |EFC Africa - Extreme Fighting Championship Africa 25 |

Now that we can cleanly combine and access fighter and fight-level data, we can dive into some deeper analyses. I have already talked about one such [analysis]({{ site.url }}/2016/05/17/smallWorldMMA/): treating bouts as an undirected graph to investigate the factors affecting fighter matchups.
