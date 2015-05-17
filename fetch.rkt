#lang racket

(require racket/match
         racket/port
         net/url
         json)

(struct mirror (url
                 score
                 delay
                 completion-percentage
                 duration/standard-deviation
                 duration/average
                 protocol
                 country
                 country-code
                 last-sync)
        #:transparent)

(define/contract (fetch-mirrors [list-url "https://www.archlinux.org/mirrors/status/json/"])
  (() (string?) . ->* . jsexpr?)

  (call/input-url (string->url list-url)
                  get-pure-port
                  read-json))

(define/contract (json-data->mirrors jsexprs)
  (jsexpr? . -> . (listof mirror?))

  (define/contract (json-item->mirror i)
    (jsexpr? . -> . mirror?)

    (match i
      [(hash-table
         ('delay delay)
         ('url url)
         ('score score)
         ('protocol protocol)
         ('country country)
         ('last_sync last-sync)
         ('completion_pct completion-percentage)
         ('duration_stddev duration/standard-deviation)
         ('duration_avg duration/average)
         ('country_code country-code))
       (mirror url
               score
               delay
               completion-percentage
               duration/standard-deviation
               duration/average
               protocol
               country
               country-code
               last-sync)]))

  (map json-item->mirror (hash-ref jsexprs 'urls)))

(define/contract (get-mirror-score m)
  (mirror? . -> . number?)

  (match (mirror-score m)
    [(? number? x) x]
    [_ +inf.0])) ; Give any 'null score infinity

(define/contract (sort-mirrors/score mirrors)
  ((listof mirror?) . -> . (listof mirror?))

  (sort mirrors < #:key get-mirror-score))

(module+ main
  (require racket/pretty)
  (pretty-print (map mirror-score
                     (sort-mirrors/score (json-data->mirrors (fetch-mirrors))))))
