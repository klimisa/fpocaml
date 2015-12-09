let proof_of_bug = 
  [| {code=0; contact={name="A"; phone_number=(1,2,3,4)}};
     {code=0; contact={name="B"; phone_number=(1,2,3,4)}};
     {code=1; contact={name="A"; phone_number=(1,2,3,4)}};
     {code=2; contact={name="B"; phone_number=(1,2,3,4)}};|] ;;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let rec aux idx =
      if db.contacts.(idx).name = contact.name 
      then idx
      else aux (idx + 1) in
    let contact_index  = aux 0 in
    let cells i =
      if i < contact_index then
        db.contacts.(i)
      else if contact_index = (Array.length db.contacts) - 1 then
        nobody
      else if (i+1) >= (Array.length db.contacts) then
        nobody
      else
        db.contacts.(i+1)
    in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact);;

let update db contact =
  let (status, db, _) = search db contact in
  if not status then 
    insert db contact
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        {name= contact.name; phone_number=contact.phone_number}
      else
        db.contacts.(i) in
    let db' = {
      number_of_contacts = db.number_of_contacts;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact);;

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;	    
