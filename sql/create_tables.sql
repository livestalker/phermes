--- таблица пользователей
--- user_id       - уникальынй идентификатор пользователя
--- username      - имя пользователя
--- password      - пароль
--- email         - адрес электронной почты
--- role          - роль (уровень доступа)
--- balance       - баланс
--- ts_created    - дата создания учетной записи
--- ts_last_login - дата последнего входа в систему
create table users (
    user_id         serial          not null,
    username        varchar(255)    not null,
    password        varchar(32)     not null,
    email           varchar(255)    not null,
    role            varchar(20)     not null,
    balance         decimal(6,2) unsigned not null default 0,
    ts_created      datetime        not null,
    ts_last_login   datetime,

    primary key (user_id),
    unique (username)
) engine = InnoDB character set utf8 collate utf8_general_ci;

--- таблица для хранения профиля пользователя
--- user_id       - уникальный идентификатор пользователя
--- profile_key   - ключ профиля
--- profile_value - значение ключа профиля
create table users_profile (
    user_id         bigint unsigned not null,
    profile_key     varchar(255)    not null,
    profile_value   text            not null,

    primary key (user_id, profile_key),
    foreign key (user_id) references users (user_id) on delete cascade
) engine = InnoDB character set utf8 collate utf8_general_ci;

--- история денежных переводов/списаний
--- transaction_id - уникальный номер транзакции
--- user_id        - уникальный идентификатор пользователя
--- type           - тип приход/расход
--- value          - сумма прихода/расхода
--- text           - описание
create table cash_history (
    transaction_id serial           not null,
    user_id     bigint unsigned     not null,
    type        varchar(10)         not null,
    value       decimal(6,2) unsigned    not null,
    text        varchar(255)        not null,

    primary key (transaction_id),
    foreign key (user_id) references users (user_id) on delete cascade
)  engine = InnoDB character set utf8 collate utf8_general_ci;

--- устройства пользователя
--- device_id - уникальный номер устройства
--- user_id   - уникальный номер пользователя
--- type      - тип устройства (модель)
--- imei      - номер IMEI
--- name      - короткое название объекта
--- text      - описание
--- longitude - текущая широта
--- latitude  - текущая долгота
--- ts_time   - последнее время запроса
create table devices (
    device_id    serial             not null,
    user_id      bigint unsigned    not null,
    type        varchar(30)        not null,
    imei         varchar(30)       not null,
    name         varchar(30)       not null,
    text         varchar(255)      not null,
    longitude    float(10, 6)      not null,
    latitude     float(10, 6)      not null,
    ts_time      datetime           not null,

    primary key (device_id),
    foreign key (user_id) references users (user_id) on delete cascade
)  engine = InnoDB character set utf8 collate utf8_general_ci;

--- история передвижения объекта
--- device_id - уникальный номер устройства
--- logitude  - широта
--- latitude  - долгота
--- ts_time   - время метки
create table track_history (
    device_id    bigint unsigned     not null,
    longitude    float(10, 6)        not null,
    latitude     float(10, 6)        not null,
    ts_time      datetime            not null,

    primary key (device_id, ts_time),
    foreign key (device_id) references devices (device_id) on delete cascade
)  engine = InnoDB character set utf8 collate utf8_general_ci;