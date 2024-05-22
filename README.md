# Novel M&V #
## Project overview ##
This repository provides analysis codes for both manuscript and supplementary materials.

Analysts typically estimate the performance impact of a change, or intervention, in a building using a process called measurement and verification (M&V). Current best practice M&V methods require an analyst to conduct a pre-/post- intervention analysis on the same performance metric (such as utility or submetered power consumption) and use the estimated difference as an indication of the intervention effect. These methods normally take a long time to yield an answer (1 - 2 years), generally lack uncertainty quantification, and are vulnerable to unexpected changes in building operation that are unrelated to the intervention. Last, the results become outdated over time. This paper describes a novel M&V method inspired by other research fields. The method applies to interventions that are easily switchable, such as control strategies. It uses a switchback design that randomly switches building operation from baseline to intervention strategy at fixed time intervals. It uses a sequential testing procedure to analyze the collected measurements in an iterative process of evaluating pre-defined stopping criteria. Upon satisfying all criteria, the analyst reports estimated savings with an associated uncertainty range. The paper demonstrates the method by estimating the energy savings of a control strategy developed by a software-as-a-service company in a case study building in Chicago, IL, USA, while providing guidance on randomization and blocking, selection of meaningful stopping criteria, implementation of sequential evaluation, and estimation of normalized annual savings. Overall, the method obtained results faster and more accurately than existing M&V methods while also providing a valid uncertainty estimate and a means of continuously updating the savings estimate over the long term.

## Highlights ##
* We identified and addressed common drawbacks of traditional M&V methods
* We developed a novel M&V method using a randomized switchback and blocking design
* A sequential test further increases the robustness of M&V savings estimation
* A case study showed improved uncertainty quantification in a much reduced timeline
* The method is applicable to a variety of performance metrics and use cases

## Graphical summary ##
![image](https://github.com/CenterForTheBuiltEnvironment/randomized_mnv/assets/86043312/ca36e5ac-d793-4a43-a547-a2d7aaa79f1f)


## Structure ##
This repository includes: 
* [readfiles](readfiles/): all input files
* [functions](functions/): defined functions for the analysis
* [docs](docs/): output documents (manuscript and example)
* [figs](figs/): generated figures for visualization
