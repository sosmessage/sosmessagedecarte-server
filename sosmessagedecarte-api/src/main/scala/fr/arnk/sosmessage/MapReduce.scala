package fr.arnk.sosmessage

object MapReduce {
  val MapReduceMessagesCollectionName = "mapReduceMessages_"

  val mapJS = """
    function() {
      emit(this._id, this);
    }
  """

  val reduceJS = """
    function(key, values) {
    }
  """

  val finalizeJS = """
    function(key, value) {
      var count = 0;
      var total = 0;
      var votePlus = 0;
      var voteMinus = 0;
      var userVote = 0;
      for (var prop in value.ratings) {
        var rating = value.ratings[prop];
        var vote = rating == 1 ? -1 : 1
        if (prop == uid) {
          userVote = vote
        }

        if (vote == 1) {
          votePlus++;
        } else {
          voteMinus++;
        }

        count++;
        total += value.ratings[prop];
      }

      value.votePlus = votePlus;
      value.voteMinus = voteMinus;
      value.userVote = userVote;

      if (total == 0 || count == 0) {
        avg = 0;
      } else {
        avg = total / count;
      }

      value.ratingCount = count;
      value.rating = avg;

      delete value.ratings;

      return value;
    }
  """
}
