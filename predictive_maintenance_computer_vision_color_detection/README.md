# Computer Vision for Predictive Maintenance (UPS Status Classification)

## Business problem
Equipment in hard-to-reach or hazardous locations can't be inspected on a routine 
schedule without cost or risk. This project replaces manual inspection with a 
camera-based status check that outputs a single actionable signal — not a score 
buried in a dashboard, but a status that directly triggers a maintenance decision.

## Approach
- Classifies UPS operational status (red/yellow/green) from image input
- Deployed on a Raspberry Pi + camera for on-site, real-time inference
- Output is designed to feed directly into a maintenance action, not a report

## Why this still holds up
Vision-based predictive maintenance has become a mainstream industrial AI pattern 
since this was built — the field has moved toward semantic segmentation (pixel-level 
defect detection) and multi-sensor fusion (thermal + visual) for more granular 
degradation tracking. The core decision this project makes — an unattended visual 
signal replacing a manual check — is the same bet the field has doubled down on.

## If I rebuilt this today
- Segmentation instead of classification, for graduated wear detection rather than 
  a 3-state signal
- Drift monitoring on the classifier itself — the visual model degrading silently 
  is exactly the "unclear filter requirements, metrics look fine until they don't" 
  failure mode I've since built evaluation practices around
- A short eval harness tracking false-positive/false-negative rates over time

## Limitations (as originally built)
- Single-device proof of concept, not fleet-scale
- No drift or retraining pipeline at build time