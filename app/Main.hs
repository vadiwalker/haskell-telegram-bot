{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import           Network.HTTP.Client      (newManager, managerSetProxy)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Client.Internal
import           Web.Telegram.API.Bot
import           Control.Concurrent
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy                  as LBS
import           Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import           Data.Text as T (pack, unpack)
import           Data.IORef
import           Database.PostgreSQL.Simple
import           GHC.Int
import           Data.List.Split

dBACK = "◀️ Назад"

main :: IO ()
main = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- newManager tlsManagerSettings

  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='users' user=postgres password=23041998v"

  ret <- runTelegramClient token manager $ do
    info <- getWebhookInfoM
    let message_request = sendMessageRequest (ChatId 295605654) "Hello"
    let get_updates_request = getUpdatesRequest
    updates <- getUpdatesM get_updates_request
    sendMessageM message_request

  case ret of 
    Left err -> print err
    Right response -> print ((chat_id (chat (result response))))

  is_work <- work 0
  print ret
  print "done!"

work :: Int -> IO ()
work offset = do

  delay <- threadDelay 1000000

  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng"
  manager <- newManager tlsManagerSettings

  upd <- runTelegramClient token manager $ do
    let get_updates_request = GetUpdatesRequest (Just offset) Nothing Nothing Nothing
    getUpdatesM get_updates_request
  
  case upd of
    Right response -> do
      let updates = result response
      let x = length updates
      print (show x)

      if x > 0 then do
        let update = (head updates)

        isUpdated <- processUpdate update
        work ((update_id update) + 1)

        else (work offset)

    _ -> print "No Value"

  print "Work done!"

processUpdate update = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng"
  manager <- newManager tlsManagerSettings

  let mes = (message update)

  case mes of
    Just m -> do
      let id = chat_id (chat m)
      let maybe_text = text m
      case maybe_text of
        Just text -> do
          unused <- process (fromIntegral id) (T.unpack text)
          
          print "Sent!"
        Nothing -> print "No message!"
    Nothing -> print "No message!"

  let cquery = (callback_query update)

  case cquery of
    Just c -> do
      let mes = (cq_message c)
      
      case mes of
        Just m -> do
          let mid = (message_id m)
          let id = (chat_id (chat m))
          let cdata = (cq_data c)
          -- print mes
          print mid
          print id
          print cdata

          case cdata of
            Just cd -> do 
              let arr_cdata = splitOn " " (T.unpack cd)

              case (arr_cdata !! 0) of
                "go_to_category" -> do
                  let category = (arr_cdata !! 1)
                  let page = (arr_cdata !! 2)
                  go_to_category id category (read page :: Int) (Just mid)
            
            Nothing -> print "No callback data!"

        Nothing -> print "Bye"
      
    Nothing -> print "Not a callback_query"

buttons0 = myReplyKeyboardMarkup [[keyboardButton "🎰 Магазин"], [keyboardButton "➕ Добавить бота"]]

cancel_button = myReplyKeyboardMarkup [[keyboardButton "🅾️ Отмена"]]

category_buttons = myReplyKeyboardMarkup [[keyboardButton "💰 ТОП Платных", keyboardButton "💥 ТОП Бесплатных"], [keyboardButton "🎮 Игры", keyboardButton "📕 Образование"], [keyboardButton "🚑 Здоровье", keyboardButton "🎶 Музыка"], [keyboardButton "🎲 Другое", keyboardButton "🎯 Спорт"], [keyboardButton dBACK]]

choose_category_buttons = myReplyKeyboardMarkup [[keyboardButton "🎮 Игры", keyboardButton "📕 Образование"], [keyboardButton "🚑 Здоровье", keyboardButton "🎶 Музыка"], [keyboardButton "🎲 Другое", keyboardButton "🎯 Спорт"], [keyboardButton "🅾️ Отмена"]]

bot_type_buttons = myReplyKeyboardMarkup [[keyboardButton "Платный", keyboardButton "Бесплатный"], [keyboardButton "🅾️ Отмена"]]

mySendMessageWithRM id text keyboard = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng"
  manager <- newManager tlsManagerSettings

  ret <- runTelegramClient token manager $ do
    let message_request = SendMessageRequest (ChatId id) text (Just HTML) Nothing Nothing Nothing (Just keyboard)
    sendMessageM message_request

  print "Message Sent!"

mySendMessage id text = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- newManager (managerSetProxy (noProxy) tlsManagerSettings)

  ret <- runTelegramClient token manager $ do
    let message_request = SendMessageRequest (ChatId id) text Nothing Nothing Nothing Nothing Nothing
    sendMessageM message_request

  print "done!"

myEditMessageTextHTML id mid text keyboard = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- newManager (managerSetProxy (noProxy) tlsManagerSettings)

  ret <- runTelegramClient token manager $ do
    let message_request = EditMessageTextRequest (Just (ChatId id)) (Just mid) Nothing text (Just HTML) Nothing (Just keyboard)
    editMessageTextM message_request

  print "done!"

mySendMessageHTML id text = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng"
  manager <- newManager (managerSetProxy (noProxy) tlsManagerSettings)

  ret <- runTelegramClient token manager $ do
    let message_request = SendMessageRequest (ChatId id) text (Just HTML) Nothing Nothing Nothing Nothing
    sendMessageM message_request
  print "HTML message sent!"

myReplyKeyboardMarkup keyboard = ReplyKeyboardMarkup keyboard (Just True) (Just False) (Just True)

get_conn = connectPostgreSQL "host='localhost' port=5432 dbname='users' user=postgres password=23041998v"

in_bd id = do
  conn <- get_conn
  xs <- query_ conn "select count(*) as count FROM users" :: IO [Only Int]

  let count = fromOnly (head xs)
  if count > 0 then return True else return False

set_flag id flag = do
  conn <- get_conn 
  rows <- execute conn "update users set flag=? WHERE id=?" (flag :: Int, id :: Int) :: IO GHC.Int.Int64
  return True

get_flag :: Int -> IO Int

get_flag id = do
  conn <- get_conn
  xs <- query conn "select flag from users WHERE id = ?" (Only id) :: IO [Only Int]
  let x = fromOnly (head xs)
  return x

set_link id link = do
  conn <- get_conn
  rows <- execute conn "update users set link=? where id=?" (link :: String, id :: Int) :: IO GHC.Int.Int64
  return True

set_category id category = do
  conn <- get_conn
  rows <- execute conn "update users set category=? where id=?" (category :: String, id :: Int) :: IO GHC.Int.Int64
  return True

set_type id type_ = do
  conn <- get_conn
  rows <- execute conn "update users set type=? where id=?" (type_ :: String, id :: Int) :: IO GHC.Int.Int64
  return True

set_cost id cost = do
  conn <- get_conn
  rows <- execute conn "update users set cost=? where id=?" (cost :: String, id :: Int) :: IO GHC.Int.Int64
  return True

set_description id description = do
  conn <- get_conn
  rows <- execute conn "update users set description=? where id=?" (description :: String, id :: Int) :: IO GHC.Int.Int64
  return True

set_title id title = do
  conn <- get_conn
  rows <- execute conn "update users set title=? where id=?" (title :: String, id :: Int) :: IO GHC.Int.Int64
  return True

get_title id = do
  conn <- get_conn
  xs <- query conn "select title from users where id = ?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

get_link id = do
  conn <- get_conn
  xs <- query conn "select link from users where id = ?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

get_description id = do
  conn <- get_conn
  xs <- query conn "select description from users where id = ?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

get_type id = do
  conn <- get_conn
  xs <- query conn "select type from users where id=?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

get_cost id = do
  conn <- get_conn
  xs <- query conn "select cost from users where id=?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

get_category id = do
  conn <- get_conn
  xs <- query conn "select category from users where id = ?" (Only id) :: IO [Only String]
  let x = fromOnly (head xs)
  return x

------------------

roll_bots_reply a = case a of
  [] -> return ""
  (a, b, c, oi, type_, cost, t) : xs -> do
    username <- get_username oi
    rem <- roll_bots_reply xs
    if type_ == "free" then return (("📌 <b>" ++ t ++ "</b>\n" ++ b ++ "\nСсылка: " ++ a ++ "\n<i>Бесплатный</i>\n\n") ++ rem) 
                                                        else return (("📌 <b>" ++ t ++ "</b>\n" ++ b ++ "\n<i>Стоимость</i>: " ++ cost ++ "Р\nСсылка: " ++ a ++ "\nВладелец: " ++ username ++ "\n\n") ++ rem) 

bots_a_page = 5

slice from to xs = take (to - from + 1) (drop from xs)

get_username id = do
  let token = Token "bot883686687:AAGd5e0mqaDkuI5ycae7xUV2C2BtjSCt9ng" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- newManager tlsManagerSettings

  ret <- runTelegramClient token manager $ do
    getChatM (T.pack (show id))
  
  case ret of
    Right response -> do
      let username = (chat_username (result response))
      case username of
        Just u -> return ("@" ++ (T.unpack u))
        Nothing -> return "@"
    _ -> return ""

get_xs category = do
  conn <- get_conn

  case category of
    "top_free" -> query_ conn "select * from bots where type='free'" :: IO [(String, String, String, Int, String, String, String)]
    "top_pay" -> query_ conn "select * from bots where type='pay'" :: IO [(String, String, String, Int, String, String, String)]
    _ -> query conn "select * from bots where category = ?" (Only category) :: IO [(String, String, String, Int, String, String, String)] 

get_bots_reply category page = do
  conn <- get_conn

  xs <- get_xs category
  let page_xs = slice (page * bots_a_page) (page * bots_a_page + bots_a_page) xs

  body <- roll_bots_reply page_xs
  let reply = "<b>" ++ (decodeCategory category) ++ "</b>\n\n" ++ body
  return reply

is_category text = (text == "📕 Образование") || (text == "🎮 Игры") || (text == "🎶 Музыка") || (text == "🚑 Здоровье") || (text == "🎲 Другое" || (text == "🎯 Спорт") || (text == "💰 ТОП Платных") || (text == "💥 ТОП Бесплатных"))

decodeCategory :: String -> String

decodeCategory category = case category of
  "education" -> "📕 Образование" 
  "games" -> "🎮 Игры" 
  "music" -> "🎶 Музыка"
  "health" -> "🚑 Здоровье"
  "other" -> "🎲 Другое"
  "sport" -> "🎯 Спорт"
  "top_free" -> "💥 ТОП Бесплатных"
  "top_pay" -> "💰 ТОП Платных"
  _ -> "Неизвестно"

encodeCategory :: String -> String

encodeCategory category = case category of
  "📕 Образование" -> "education"
  "🎮 Игры" -> "games"
  "🎶 Музыка" -> "music"
  "🚑 Здоровье" -> "health"
  "🎲 Другое" -> "other"
  "🎯 Спорт" -> "sport"
  "💥 ТОП Бесплатных" -> "top_free"
  "💰 ТОП Платных" -> "top_pay"
  _ -> "unknown"  

post_bot id = do
  link <- get_link id
  description <- get_description id
  category <- get_category id
  type_ <- get_type id
  cost <- get_cost id
  title <- get_title id

  conn <- get_conn

  execute conn "insert into bots (link, description, category, owner_id, type, cost, title) VALUES (?,?,?,?,?,?,?)" (link, description, category, id, type_, cost, title) :: IO GHC.Int.Int64
  return True

go_to_shop id = do
  unused <- set_flag id 2
  mySendMessageWithRM (fromIntegral id) "<b>🎰 Магазин</b> \n\n👇 Выберите категорию!" category_buttons

go_to_main id = do
  unused <- set_flag id 0
  mySendMessageWithRM (fromIntegral id) "<b>Главное меню</b>" buttons0

get_count_pages category = do
  conn <- get_conn

  case category of
    "top_free" -> query_ conn "select count(*) as count FROM bots where type='free'" :: IO [Only Int]
    "top_pay" -> query_ conn "select count(*) as count FROM bots where type='pay'" :: IO [Only Int]
    _ -> query conn "select count(*) as count FROM bots where category=?" (Only category) :: IO [Only Int]

get_pages category = do
  conn <- get_conn
  xs <- get_count_pages category

  let count = fromOnly (head xs)
  return (quot (count + bots_a_page - 1) bots_a_page)

go_to_category id category page mid = do
  reply <- get_bots_reply category page
  buttons <- inline_page_buttons category page

  case mid of
    Just m -> myEditMessageTextHTML (fromIntegral id) m (T.pack reply) (InlineKeyboardMarkup buttons) 
    Nothing -> mySendMessageWithRM (fromIntegral id) (T.pack reply) (ReplyInlineKeyboardMarkup buttons)

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

dataInlineButton text callback_data = InlineKeyboardButton text Nothing (Just callback_data) Nothing Nothing Nothing Nothing

collect_page_buttons i page pages category = do
  if i == pages then do
    return []
  else do
    if i == page then do
      next <- collect_page_buttons (i + 1) page pages category
      return ((dataInlineButton (T.pack ("· " ++ (show (page + 1)) ++ " ·")) "no_reply") : next)
    else do
      if i == 0 then do
        next <- collect_page_buttons (i + 1) page pages category
        return ((dataInlineButton (T.pack ("« " ++ (show 1))) (T.pack ("go_to_category " ++ category ++ " " ++ (show 0)))) : next) 
      else do
        if i == pages - 1 then do
          next <- collect_page_buttons (i + 1) page pages category
          return ((dataInlineButton (T.pack (((show pages)) ++ " »")) (T.pack ("go_to_category " ++ category ++ " " ++ (show (pages - 1))))) : next)
        else do
          if (i < page) && (page - i) <= (max 1 (4 - pages + page)) then do
             next <- collect_page_buttons (i + 1) page pages category
             return ((dataInlineButton (T.pack ("‹ " ++ (show (i + 1)))) (T.pack ("go_to_category " ++ category ++ " " ++ (show i)))) : next)
          else do
            if (i > page) && (i - page) <= (max 1 (3 - page)) then do
              next <- collect_page_buttons (i + 1) page pages category
              return ((dataInlineButton (T.pack (((show (i + 1))) ++ " ›")) (T.pack ("go_to_category " ++ category ++ " " ++ (show i)))) : next)
            else do
              collect_page_buttons (i + 1) page pages category



inline_page_buttons :: String -> Int -> IO [[InlineKeyboardButton]]

inline_page_buttons category page = do
  pages <- get_pages category
  rec_buttons <- collect_page_buttons 0 page pages category
  let ret = [rec_buttons]
  return ret

process id text = do
  conn <- get_conn
  chat <- get_username id

  if text == "/start" then do
    in_bd_x <- in_bd id
    if not in_bd_x then do

      execute conn "insert into users (id, flag) values (?, ?)" (id :: Int, 0 :: Int) :: IO GHC.Int.Int64
      print (show "Not in bd!")

    else print "In bd!"

    set_flag id 0
    mySendMessageWithRM (fromIntegral id) "Приветствую! ✋ \nЗдесь вы найдёте то, что ищете :) \n\n<b>Поехали!</b> 🚀" buttons0

  else do
    flag <- get_flag id

    case flag of
      0 -> case text of
        "🎰 Магазин" -> go_to_shop id
        "➕ Добавить бота" -> do
          _ <- set_flag id 7
          mySendMessageWithRM (fromIntegral id) "Введите название бота" cancel_button
        _ -> print "Not in case!"

      1 -> case text of 
        "🅾️ Отмена" -> go_to_main id

        _ -> do
          _ <- set_link id text
          _ <- set_flag id 3
          mySendMessage (fromIntegral id) "Отлично, Введите описание бота"

      2 -> case text of
        "◀️ Назад" -> go_to_main id
        _ -> if is_category text then go_to_category id (encodeCategory text) 0 Nothing else print "Not in case!"

      3 -> case text of
        "🅾️ Отмена" -> go_to_main id

        _ -> do
          _ <- set_description id text
          _ <- set_flag id 4
          mySendMessageWithRM (fromIntegral id) "👇 Выберите самую подходящую категорию!" choose_category_buttons

      4 -> case text of
        "🅾️ Отмена" -> go_to_main id

        _ -> do
          if is_category text then do
            let category = text
            _ <- set_category id (encodeCategory category)
            _ <- set_flag id 5
            mySendMessageWithRM (fromIntegral id) "Ваш бот платный или бесплатный?" bot_type_buttons
            
            ----------

          else mySendMessage (fromIntegral id) "Внимание! Выберите одну из категорий, пожалуйста!"

      5 -> case text of
        "🅾️ Отмена" -> go_to_main id

        "Платный" -> do
          _ <- set_type id "pay"
          _ <- set_flag id 6
          mySendMessageWithRM (fromIntegral id) "Введите стоимость бота (Р)" cancel_button

        "Бесплатный" -> do
          _ <- set_type id "free"
          _ <- post_bot id
          mySendMessage (fromIntegral id) "Бот успешно размещён! ✅"
          go_to_main id

        _ -> mySendMessage (fromIntegral id) "Выберите один из вариантов, пожалуйста"

      6 -> case text of
        _ -> if not (isInteger text) then mySendMessage (fromIntegral id) "Введите только число!" else do
          _ <- set_cost id text
          _ <- post_bot id
          mySendMessage (fromIntegral id) "Бот успешно размещён! ✅"
          go_to_main id

      7 -> case text of
        "🅾️ Отмена" -> go_to_main id

        _ -> do
          _ <- set_title id text
          _ <- set_flag id 1
          mySendMessageWithRM (fromIntegral id) "Введите ссылку бота \nНапример: @TinderGramRobot" cancel_button

      _ -> print "String"
