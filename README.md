# NEST v1.1
The Nexus Solutions Tool (NEST) v1.1 is a new open modeling platform that integrates multi-scale energy-water-land resource optimization with distributed hydrological modeling. This approach provides insights into the vulnerability of water, energy and land resources to future socioeconomic and climatic change and how multi-sectoral policies, technological solutions and investments can improve the resilience and sustainability of transformation pathways. 

Detailed description is avaialble at Vinca et al. (2020) _Geoscientific Model Development_
[https://doi.org/10.5194/gmd-13-1095-2020](https://doi.org/10.5194/gmd-13-1095-2020)

## Content
This repositories contains the code of the two key models linked in this framework:

- The Community Water Model, a hydrological model (see [Full documentation and open repository](https://cwatm.iiasa.ac.at/) )
- MESSAGEix, dynamic systems-optimization Integrated Assessment Model (see [Documentation](https://messageix.iiasa.ac.at/) )

# Updates in NEST v1.1

This version contains minor updates to the model policy setup and input data to align with the scenario described in the following paper:

"Trans-boundary cooperation facilitates sustainable development in the Indus Basin", Vinca et el. (2020), NAture Sustainability, in review.

Updates include:
- water and irrigation technologies used for three main scenarios (BAU, SDG and SDG-coop)
- edits to `multiple_scenario_new.r` to include new scenarios
- fixed bugs that were overwriting local paths and input data

# Data
Due to data sharing restrictions, procesed data used as model input is included in `MESSAGEix\NEST_Indus_SSP2_RPC6.RData`
## License

The NEST framework is licensed under the Apache License, Version 2.0 (the
"License"); you may not use the files in this repository except in compliance
with the License. You may obtain a copy of the License at
<http://www.apache.org/licenses/LICENSE-2.0>.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
