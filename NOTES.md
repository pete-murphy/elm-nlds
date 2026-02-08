What is this doing? could we just bake this weighting into the parsers (or into parseActivitySmart somehow?) -- would that simplify things?

```elm
  scored =
      List.map
          (\r ->
              let
                  durationScore =
                      if r.duration /= Nothing then
                          10

                      else
                          0

                  timeScore =
                      case r.timeSpec of
                          Nothing ->
                              0

                          Just ts ->
                              scoreTimeSpec ts
              in
              ( durationScore + timeScore, r )
          )
          results
```
