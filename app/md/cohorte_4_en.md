---
title: "DYPOP"
author: "Victor Bret & Fabrice Zaoui"
output: html_document
---

To predict the densities of adults (> 1+) likely to be observed, survival must be predicted:
- **observed adults the previous year**, still alive in the station;
- and **observed 1+ the previous year** that became adults in the considered year.

The **prediction of the adult densities represented in three dimensions**, using a heat map (by default) or in relief (by checking the "3D view" box). For easier reading, only the **middle prediction is shown on the central panel**.

The upper and right panels represent the marginal relationships:
- **Upper panel**: evolution of the adult densities according to the observed adult densities the previous year (the observed 1+ density the previous year is fixed, indicated on the scale at the bottom right previous year is fixed, indicated on the scale at the bottom right);
- **Right panel**: evolution of the adult densities according to the observed 1+ densities the previous year (the observed adult density the previous year is fixed, indicated on the scale at the bottom left).

These marginal visions represent the range of uncertainty around these predictions:
- The red line is to the median prediction;
- 50% of the predicted densities are contained in the dark gray range;
- 95% of the predicted densities are contained in the light gray range.

