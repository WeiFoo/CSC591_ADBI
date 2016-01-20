
from pyspark import SparkConf, SparkContext
from pyspark.streaming import StreamingContext
from pyspark.streaming.kafka import KafkaUtils
import operator
import string
import numpy as np
import matplotlib.pyplot as plt
import pdb

def main():
    conf = SparkConf().setMaster("local[2]").setAppName("Streamer")
    sc = SparkContext(conf=conf)
    ssc = StreamingContext(sc, 10)   # Create a streaming context with batch interval of 10 sec
    ssc.checkpoint("checkpoint")

    pwords = load_wordlist("positive.txt")
    nwords = load_wordlist("negative.txt")

    counts = stream(ssc, pwords, nwords, 100)
    make_plot(counts)


def make_plot(counts):
    """
    Plot the counts for the positive and negative words for each timestep.
    Use plt.show() so that the plot will popup.
    """
    # YOUR CODE HERE
    pos_count, neg_count = [],[]

    for count in counts:
        if count:
            pos_count.append(count[0][1])
            neg_count.append(count[1][1])
    ax=plt.subplot(111)
    print len(pos_count)
    x= range(0,len(pos_count))
    # pdb.set_trace()
    ax.plot(x,pos_count,'bo-',label = "positive")
    ax.plot(x,neg_count,'go-', label = "negative")
    y_max = max(max(pos_count),max(neg_count))+50
    ax.set_ylim([0,y_max])
    plt.xlabel("Time step")
    plt.ylabel("Word count")
    plt.legend(fontsize = 'small',loc=0)
    plt.savefig("plot.png")
    plt.show()




def load_wordlist(filename):
    # pdb.set_trace()
    # print "hello!~~~"

    """
    This function should return a list or set of words from the given filename.
    """
    # YOUR CODE HERE
    words = []
    with open(filename,mode='r') as f:
        words = f.readlines()
    return [ i.strip() for i in words]




def stream(ssc, pwords, nwords, duration):
    def counts(line):
        pos_count, neg_count = 0,0
        # words_raw = line.split()
        words =[ word.strip(string.punctuation).lower() for word in line.split()]
        for word in words:
            if word in pwords:
                pos_count +=1
            if word in nwords:
                neg_count +=1
        return [("positive",pos_count),("negative",neg_count)]


    kstream = KafkaUtils.createDirectStream(
        ssc, topics = ['twitterstream'], kafkaParams = {"metadata.broker.list": 'localhost:9092'})
    tweets = kstream.map(lambda x: x[1].encode("ascii","ignore"))
    words = tweets.flatMap(counts).\
                   reduceByKey(lambda x,y:x+y)
    words.pprint()
    # pdb.set_trace()
    # tweets_pn = words.map(lambda x:("positive",1) if x in pwords else ("negative", 1) if x in nwords else ("other",1))
    # tweets_pn.pprint()
    # pdb.set_trace()
    # Each element of tweets will be the text of a tweet.
    # You need to find the count of all the positive and negative words in these tweets.
    # Keep track of a running total counts and print this at every time step (use the pprint function).
    # YOUR CODE HERE


    # Let the counts variable hold the word counts for all time steps
    # You will need to use the foreachRDD function.
    # For our implementation, counts looked like:
    #   [[("positive", 100), ("negative", 50)], [("positive", 80), ("negative", 60)], ...]
    counts = []
    # YOURDSTREAMOBJECT.foreachRDD(lambda t,rdd: counts.append(rdd.collect()))
    words.foreachRDD(lambda t, rdd:counts.append(rdd.collect()))
    ssc.start()                         # Start the computation
    ssc.awaitTerminationOrTimeout(duration)
    ssc.stop(stopGraceFully=True)

    return counts


if __name__=="__main__":
    main()
