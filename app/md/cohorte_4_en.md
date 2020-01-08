---
title: "DYPOP"
author: "Victor Bret & Fabrice Zaoui"
output: html_document
---

To predict the densities of adults (> 1+) likely to be observed, survival must be predicted:
- **adults observed the previous year** who will still be alive within the station;
- and **1 + observed in the previous year** who became adults in the year considered.

The **prediction of the densities of adults in place is therefore to be represented in three dimensions**, using a heat map (by default) or in relief (by checking the "3D view" box). For easier reading, only the **middle prediction is shown on the central panel**.

The upper and right panels represent the marginal relationships:
- **Upper panel**: evolution of the densities of adults according to the densities of adults observed the previous year (the density of 1+ observed the previous year is fixed, indicated on the scale at the bottom right);
- **Right panel**: evolution of the densities of adults according to the densities of 1+ observed the previous year (the density of adults observed the previous year is fixed, indicated on the scale at the bottom left).

These marginal visions make it possible to represent the envelope of uncertainty around these predictions:
- The red line corresponds to the median prediction;
- 50% of the densities predicted by the model are contained in the dark gray envelope;
- 95% of the densities predicted by the model are contained in the light gray envelope.

