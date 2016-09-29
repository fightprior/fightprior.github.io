---
title: "MMA styles I: grouping finishes using large-scale data"
description: "Constructing a network of MMA fighting styles"
author: sean
layout: post
comments: true
hidden: true
---
<!--more-->

MMA fighters use a mixture of martial arts and the way that they win fights reflects their personal style.

A useful way of understanding the styles of fighters is if we (1) determine the styles of individual fighters and, (2) understand the set of techniques that an individual style entails. To investigate the latter question, rather than assuming that the grouping of techniques was known (such as heel hooks and triangle chokes contributing to a BJJ game), I determined whether such groupings could naturally emerge based on how frequently finishes have been used by the same fighters (i.e. do fighters who win with armbars tend to win with triangle chokes?)

Using data from ~120,000 wins across 35,000 fighters, I determined how frequently each pair of [50 common finishes]({{ site.url }}/2016/05/05/summarizingFights/) was used by the same fighters relative to chance, allowing me to identify ~300 pairs of finishes that are commonly used together by fighters. These finish pairs can be used to more broadly organize finishes, grouping finishes that share similar partners.

![Fighter scraping strategy]({{ site.url }}/figure/source/2016-09-29-finishCooccurrence/finish_network-1.png){: .align-center }

From this analysis, I find that rather than grouping finishes as stand-alone sets, fnishes form a continuity of MMA styles: ranging from pure striking to pure BJJ, connected by submissions that require strong positional control. There are many fine-grain patterns in this analysis. For example, all BJJ submissions that target the legs are tightly intermingled; if someone is dangerous with ankle locks, he/she is also dangerous with heel hooks.

If you are interested in better understanding the computational and statistical elements of this analysis, see the [Full Quantitative Analysis]({{ site.url }}/2016/09/29/finishCooccurrence).