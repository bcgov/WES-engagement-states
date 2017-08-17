# WES /RSHINY DEVOPS

Running templates
----------------
All of the following instructions have the following prerequisites:

1. The "oc" command is installed properly and can be run from any directory
2. The operator has Admin access to OpenShift as has logged in
3. The `oc project` command shows that the operator is in the correct project
3. The Git repository for WES-engagement-states has been cloned to the operator's computer
4. The operator has changed to the directory containing the templates (tools)


Build Project
-------------

1. Go to the OpenShift Project Area
2. Add to project
3. Import YAML/JSON
4. Choose from your local repo\tools\openshift-template.json
5. Click Create
6. Click Process the Template

In the Template:
1. Update Name
2. Update Git URL
3. Update R Packages to: ggplot2 dplyr DT car readr networkD3  pander tidyr
4. Update R GitHub Packages to: igraph/rigraph ropensci/plotly@a1613b3e225
