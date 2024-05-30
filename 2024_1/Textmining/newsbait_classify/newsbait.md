낚시성 기사 탐지 모델 개발

1. Ai-Hub 에서 낚시성 기사 데이터 다운.
2. newscontent 와 clickbaitClass 만 추출 및 전처리 진행.
3. train : valid = 8 : 2 비율
4. Random Forest, LSTM, Logistic Regression 모델 학습 진행.
5. 정확성 측면에서는 RF, LSTM 이 높음. 하지만 새로운 기사 예측 진행 시 예측 확률이 높은 것은 RF 모델로 학습한 경우.
6. 새로운 기사 데이터는 서울대학교 언론정보연구소인 SNU 에서 수집.
