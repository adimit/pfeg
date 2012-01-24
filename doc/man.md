# Predicting Functional Elements in German

## Configuration and Training

### Shards

The *WAC corpora are delivered piecemeal in what this program considers
"shards." The current configuration always holds a list of `testShards`, which
are not considered for lookups. This is to avoid testing on the training data.

Which shard a particular example originates from is determined during runtime
of `recordContext` as its third argument. Shards are integer values.

