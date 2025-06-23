<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->

critical_minerals
============================

### Usage

1) When new data arrives from mine, you need to write code to clean data into a tibble with columns
- non_standard_job_title (the job titles from the mine that do not match the standard job titles) 
- mine_type (open pit or underground)
- location (mine or mill)
- staff (the number of people with that job title at that location)

2) run the code, inspect the output.  The mapping likely failed in a majority of the cases.

3) add the new job titles to the mapping file: data/mapping/add_new_job_titles_to_this_file.csv

4) re-run the code, inspect the output.  Repeat until the mapping is complete.

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/critical_minerals/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
