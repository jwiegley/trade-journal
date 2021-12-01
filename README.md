# trade-journal

This project contains a library and an executable for managing and tracking
trades placed with a broker (buy and sells of different instruments), and
other events that may occur as a result of having an account with that broker
(interest, dividends, etc).

Currently, the code is organized around a multi-stage flow of data from the
broker:

  1. Get raw CSV import from the broker.

  2. Convert CSV data into a stream of Entry values. There should be a test
     for every possible variation of CSV datum.

  3. Calculate the Open and Close disposition of buys and sells implied by
     that stream of Entry values.

  4. Calculate the profit and loss implied by those Open and Close events.

  5. Adjust this profit and loss by applying any tax-related laws. This may
     not apply for every country, and may apply differently based on the
     instrument itself.

Each segment in this flow is implemented as a Pipe.
