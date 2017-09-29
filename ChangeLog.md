# 1.1.0

## Breaking Changes

* `ListenData` no longer has an `Ord` instance. This is due to `TrackMetadata` losing its `Ord` instance.
* `TrackMetadata` has gained `trackReleaseName` and `trackAdditionalInfo` fields.
* `TrackMetadata` no longer has an `Ord` instance, as `trackAdditionalInfo` is an Aeson `Object` that lacks `Ord`.

## Other Changes

* Documentation!


# 1.0.1

Export a few more symbols


# 1.0.0

Initial release
