---INSERT INTO `users` (`user_id`, `username`, `password`, `email`, `role`, `balance`, `ts_created`, `ts_last_login`) VALUES
---(1, 'aleksio', 'c4ca4238a0b923820dcc509a6f75849b', 'mi.aleksio@mail.ru', 'member', 0, '2010-09-03 09:14:26', NULL);

---INSERT INTO `tracker`.`devices` (`device_id`, `user_id`, `type`, `imei`, `name`, `text`, `longitude`, `latitude`, `ts_time`) VALUES
---(NULL, '1', 'tk102', '5464564746747', 'машина', 'описание', 49.666325, 58.598518, '2010-09-03 09:14:26'),
---(NULL, '1', 'tk102', '3463463634646', 'кошка', 'описание', 49.614361, 58.603294, '2010-09-03 09:14:26'),
---(NULL, '1', 'tk102', '5675678567456', 'мышка', 'описание', 58.603294, 49.614361, '2010-09-03 09:14:26');

---INSERT INTO `track_history` (`device_id`, `longitude`, `latitude`, `ts_time`) VALUES
---(1, 49.666325, 58.598518, '2010-09-03 09:14:26'),
---(2, 49.614361, 58.603294, '2010-09-03 09:14:26'),
---(3, 58.603294, 49.614361, '2010-09-03 09:14:26');