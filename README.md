# geNet
The geNet tool is an R package that aims to generate an interactive network of genes in which each node is a gene and
each edge measures the intensity of the relationship between two nodes.
The geNet package has two main components:
1. The geNet algorithm:
- Generating weightened connections of genes based on their occurences across several strains.
- Clustering the nodes based on these weightened connections.
2. A visualization framework to easy navigate large networks of genes.

The visualization framework includes a separate shiny app built on top of the geNet visualization framework functions
that allows an interactive exploration of the network.
You can find additional documentation in the introduction_geNet folder: introduction_to_geNet.pdf, geNet_functions.pdf
