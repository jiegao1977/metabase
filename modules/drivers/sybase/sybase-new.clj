;;https://github.com/metabase/metabase/pull/8074/commits/452de2f5ac02d5e51cd4294b9321cf0a0bb4986d
(ns metabase.driver.sybase
  "Driver for ASE Sybase databases. Uses the jTDS driver under the hood."
  (:require [honeysql.core :as hsql]
            [metabase
             [config :as config]
             [driver :as driver]
             [util :as u]]
            [metabase.driver
             [common :as driver.common]
             [sql :as sql]]
            [metabase.driver.sql
             [query-processor :as sql.qp]]
            [metabase.driver.sql-jdbc
             [common :as sql-jdbc.common]
             [connection :as sql-jdbc.conn]
             [execute :as sql-jdbc.execute]
             [sync :as sql-jdbc.sync]]
            [metabase.util
             [honeysql-extensions :as hx]
             [ssh :as ssh]]
            [metabase.query-processor.interface :as qp.i])
  (:import [java.sql DatabaseMetaData PreparedStatement ResultSet Connection Time Types]
           [java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime OffsetTime ZonedDateTime]
           [java.time.temporal Temporal]
           [java.util TimeZone]))

(driver/register! :sybase, :parent :sql-jdbc)


(defmethod sql-jdbc.sync/database-type->base-type :sybase [_ database-type]
  ({:bigint                      :type/BigInteger
    :binary                      :type/*
    :bit                         :type/Boolean              ; actually this is 1 / 0 instead of true / false ...
    :char                        :type/Text
    :cursor                      :type/*
    :date                        :type/Date
    :datetime                    :type/DateTime
    :datetime2                   :type/DateTime
    :datetimeoffset              :type/DateTime
    :decimal                     :type/Decimal
    :float                       :type/Float
    :geography                   :type/*
    :geometry                    :type/*
    :hierarchyid                 :type/*
    :image                       :type/*
    :int                         :type/Integer
    :money                       :type/Decimal
    :nchar                       :type/Text
    :ntext                       :type/Text
    :numeric                     :type/Decimal
    :nvarchar                    :type/Text
    :real                        :type/Float
    :smalldatetime               :type/DateTime
    :smallint                    :type/Integer
    :smallmoney                  :type/Decimal
    :sql_variant                 :type/*
    :table                       :type/*
    :text                        :type/Text
    :time                        :type/Time
    :timestamp                   :type/*                    ; not a standard SQL timestamp, see https://msdn.microsoft.com/en-us/library/ms182776.aspx
    :tinyint                     :type/Integer
    :uniqueidentifier            :type/UUID
    :varbinary                   :type/*
    :varchar                     :type/Text
    :xml                         :type/*
    (keyword "numeric identity") :type/Integer} database-type))



(defmethod sql-jdbc.conn/connection-details->spec :sybase
  [_ {:keys [user password db host port instance domain ssl]
      :or   {user "dbuser", password "dbpassword", db "", host "localhost", port 1433}
      :as   details}]
  (-> {:classname   "net.sourceforge.jtds.jdbc.Driver"      ;; must be in classpath
       :subprotocol "jtds:sybase"
       :subname     (str "//" host ":" port "/" db)         ;; :subname (str "//" host "/" dbname)}   (str "//" host ":" port "/" (or dbname db))}
       :user        user
       :password    password
       ;:instance     instance
       }
      (sql-jdbc.common/handle-additional-options details, :seperator-style :url)))


(defmethod sql-jdbc.execute/prepared-statement :sybase
  [driver ^Connection conn ^String sql params]
  (let [stmt (.prepareStatement conn sql
                                ResultSet/TYPE_FORWARD_ONLY
                                ResultSet/CONCUR_READ_ONLY)]
    (try
      (.setFetchDirection stmt ResultSet/FETCH_FORWARD)
      (sql-jdbc.execute/set-parameters! driver stmt params)
      stmt
      (catch Throwable e
        (.close stmt)
        (throw e)))))



(defmethod sql.qp/apply-top-level-clause [:sybase :limit]
  [_ _ honeysql-form {value :limit}]
  (assoc honeysql-form :modifiers [(format "TOP %d" value)]))

(defmethod sql.qp/apply-top-level-clause [:sybase :page]
  [_ _ honeysql-form {{:keys [items page]} :page}]
  (assoc honeysql-form :offset (hsql/raw (format "%d ROWS FETCH NEXT %d ROWS ONLY"
                                                 (* items (dec page))
                                                 items))))

;; From the dox:
;;
;; The ORDER BY clause is invalid in views, inline functions, derived tables, subqueries, and common table
;; expressions, unless TOP, OFFSET or FOR XML is also specified.
;;
;; To fix this we'll add a max-results LIMIT to the query when we add the order-by if there's no `limit` specified,
;; but not for `top-level` queries (since it's not needed there)
(defmethod sql.qp/apply-top-level-clause [:sybase :order-by]
  [driver _ honeysql-form {:keys [limit], :as query}]
  (let [add-limit?    (and (not limit) (pos? sql.qp/*nested-query-level*))
        honeysql-form ((get-method sql.qp/apply-top-level-clause [:sql-jdbc :order-by])
                       driver :order-by honeysql-form query)]
    (if-not add-limit?
      honeysql-form
      (sql.qp/apply-top-level-clause driver :limit honeysql-form (assoc query :limit qp.i/absolute-max-results)))))


(defn- date-part [unit expr]
  (hsql/call :datepart (hsql/raw (name unit)) expr))

(defn- date-add [unit & exprs]
  (apply hsql/call :dateadd (hsql/raw (name unit)) exprs))


(defn- to-start-of-hour [expr]
  (hx/->datetime (hx/format "yyyy-MM-dd HH:00:00" expr)))

(defn- to-start-of-minute [expr]
  (hx/cast :smalldatetime expr))

(defn- to-start-of-day [expr]
  (hx/->datetime (hx/->date expr)))

;; The equivalent SQL looks like:
;;     CAST(DATEADD(day, 1 - DATEPART(weekday, %s), CAST(%s AS DATE)) AS DATETIME)
(defn- to-start-of-week [expr]
  (hx/->datetime
    (date-add :day
              (hx/- 1 (date-part :weekday expr))
              (hx/->date expr))))

(defn- to-start-of-quarter [expr]
  (date-add :quarter
            (hx/dec (date-part :quarter expr))
            (hx/format "yyyy-01-01" expr)))


(defn- to-start-of-month [expr]
  (hx/->datetime (hx/format "yyyy-MM-01" expr)))

(defn- to-start-of-year [expr]
  (date-part :year expr))

(defn- to-minute [expr]
  (date-part :minute expr))

(defn- to-hour [expr]
  (date-part :hour expr))

(defn- to-day-of-week [expr]
  (date-part :weekday expr))

(defn- to-day-of-month [expr]
  (date-part :day expr))

(defn- to-day-of-year [expr]
  (date-part :dayofyear expr))

(defn- to-week-of-year [expr]
  (date-part :iso_week expr))

(defn- to-month-of-year [expr]
  (date-part :month expr))

(defn- to-quarter-of-year [expr]
  (date-part :quarter expr))


(defmethod sql.qp/date [:sybase :default]         [_ _ expr] expr)
(defmethod sql.qp/date [:sybase :minute]          [_ _ expr] (to-start-of-minute expr))
(defmethod sql.qp/date [:sybase :minute-of-hour]  [_ _ expr] (to-minute expr))
(defmethod sql.qp/date [:sybase :hour]            [_ _ expr] (to-start-of-hour expr))
(defmethod sql.qp/date [:sybase :hour-of-day]     [_ _ expr] (to-hour expr))
(defmethod sql.qp/date [:sybase :day-of-week]     [_ _ expr] (to-day-of-week expr))
(defmethod sql.qp/date [:sybase :day-of-month]    [_ _ expr] (to-day-of-month expr))
(defmethod sql.qp/date [:sybase :day-of-year]     [_ _ expr] (to-day-of-year expr))
(defmethod sql.qp/date [:sybase :week-of-year]    [_ _ expr] (to-week-of-year expr))
(defmethod sql.qp/date [:sybase :month]           [_ _ expr] (to-start-of-month expr))
(defmethod sql.qp/date [:sybase :month-of-year]   [_ _ expr] (to-month-of-year expr))
(defmethod sql.qp/date [:sybase :quarter-of-year] [_ _ expr] (to-quarter-of-year expr))
(defmethod sql.qp/date [:sybase :year]            [_ _ expr] (to-start-of-year expr))
(defmethod sql.qp/date [:sybase :day]             [_ _ expr] (to-start-of-day expr))
(defmethod sql.qp/date [:sybase :week]            [_ _ expr] (to-start-of-week expr))
(defmethod sql.qp/date [:sybase :quarter]         [_ _ expr] (to-start-of-quarter expr))


;; Sybase doesn't support `TRUE`/`FALSE`; it uses `1`/`0`, respectively; convert these booleans to numbers.
(defmethod sql.qp/->honeysql [:sybase Boolean]
  [_ bool]
  (if bool 1 0))

(defmethod sql.qp/->honeysql [:sybase Time]
  [_ time-value]
  (hx/->time time-value))

;(defn- string-length-fn [field-key]
;  (hsql/call :len (hx/cast :VARCHAR field-key)))


;(def ^:private sqlserver-date-formatters (driver/create-db-time-formatters "yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSZ"))
;(def ^:private sqlserver-db-time-query "select CONVERT(nvarchar(30), SYSDATETIMEOFFSET(), 127)")

