# Introduction
이 어플리케이션은 **Latent Space Item Response Model (LSIRM)**을 사용하여 **이분형 문항반응자료(Item Response Data)**를 분석을 해주는 도구입니다. 이를 통해 연구자들은 데이터를 보다 쉽게 분석할 수 있으며, 여러 연구자가 동일한 조건에서 분석할 수 있어 연구 일관성을 확보할 수 있습니다.

## 사용 방법
- **Step 1. Data Upload & Visualization**
  - 데이터를 업로드 합니다. 내장된 예시 데이터를 사용 할 수 있습니다.
  - 결과
      - Visualization of Network Structure (using D3.js)
      - Network Statistic
- **Step 2. Fitting LSIRM**
    - 원하는 모형 옵션을 선택하고 "START" 버튼을 누릅니다. 선택하지 않으면 기본 옵션이 적용됩니다.
    - 결과
        - Model Summary
        - Interaction Plot
- **Step 3. Model Diagnostic**
    - Step 2에서 적합된 모형의 모형 진단 결과를 확인할 수 있습니다.
    - 결과
        - Trace Plot
        - ACF(Auto Correlation Function)
        - ROC Curve
- **Step 4. Clustering**
    
    **[주의사항]** Step 2의 "START" 버튼을 누르지 않으면 작동하지 않습니다.
    
    - 최적의 군집 수를 얻기위한 BIC 반복 수를 선택하고 "START" 버튼을 누릅니다.
    - Step 2의 결과를 기반으로 항목을 군집화 합니다.
    - 결과
        - Interaction Plot
        - Clustering Result
