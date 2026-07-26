;;; --weather-forecast__eww_forecast_weather@@20260725T231111.el --- weather-forecast -*- lexical-binding: t -*-

;;; Commentary:
;; title: weather-forecast
;; keywords: :eww:forecast:weather:
;; date: [2026-07-25 Sat 23:11]
;; identifier: 20260725T231111

;;; Code:
(defun dunc/weather-forecast ()
  (interactive)
  (eww "https://brolly.sh/forecast/ENreokTt"))

(provide '--weather-forecast__eww_forecast_weather@@20260725T231111)
;;; --weather-forecast__eww_forecast_weather@@20260725T231111.el ends here
