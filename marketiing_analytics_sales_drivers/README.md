# Marketing Mix Drivers: What Actually Moves Sales on Amazon

An early example of the driver-decomposition work behind my current focus on 
Marketing Mix Modeling — this project asked which factors were actually driving 
product sales for a major CPG brand, versus which were just correlated with it.

**Business question:** Across a multi-product Amazon catalog, which factors — 
ranking position, web performance, shipment timing, product attributes — were 
actually driving sales, and which were noise?

**Approach:** K-means clustering to segment products by behavior, PCA to collapse 
correlated ranking/performance signals into interpretable components, and random 
forest variable importance to isolate real drivers from correlated noise.

**Note:** Original data was proprietary; not included in this repo.

**Update:** Precursor to the causal/BSTS driver-decomposition methodology. 