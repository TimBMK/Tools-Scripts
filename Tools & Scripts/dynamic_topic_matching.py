# -*- coding: utf-8 -*-

# Dynamic Community matching by:
# Lorenz, Philipp et al. 2018. “Capturing the Dynamics of Hashtag-Communities.” In Complex Networks & Their Applications VI, Studies in Computational Intelligence, eds. Chantal Cherifi, Hocine Cherifi, Márton Karsai, and Mirco Musolesi. Cham: Springer International Publishing, 401–13.
# https://github.com/philipplorenz/memory_community_matching
# install / pull from git via "git clone https://github.com/philipplorenz/memory_community_matching" in terminal

import memory_community_matching
import pandas
import pickle
import math

def dynamic_topic_matching(topic_graph, n_lookback, output_file):
  # read csv
  snapshots = pandas.read_csv(topic_graph)
  
  # wrangle data into shape
  # timeseries = dict(tuple(snapshots.groupby(["time", "community"])))
  timeseries = snapshots.groupby(snapshots['time'], as_index=True).apply(lambda g: g.groupby('community')['node'].agg(set).to_dict()).to_list()
  
  # community matching
  temporal_communities = memory_community_matching.matching(timeseries, n_lookback) 
    # output: every list entry is a temporal community, consisting of communities from the snapshots. Every item in the list entry conists of: (snapshot nr, snapshot community)
    # This does NOT contain node names - nodes within the temporal communities need to be matched back later through snapshot/community information
  
  # # pickle for safe keeping
  # file = open("topics_clusters_weekly.obj", "wb")
  # pickle.dump(temporal_communities, file)
  
  
  # filehandler = open("topics_clusters_weekly.obj", "rb")
  # temporal_communities = pickle.load(filehandler)
  
  # convert to dataframe
  temporal_df = pandas.DataFrame()
  for i in range(1, len(temporal_communities)):
    df = pandas.DataFrame(temporal_communities[i], columns =  ["snapshot", "community"])
    df = df.assign(temporal_community = i)
    temporal_df = pandas.concat([temporal_df, df])
    
  
  # export as .csv
  temporal_df.to_csv(output_file)

