package com.atorres.nttdata.prodpasivems.repository;

import com.atorres.nttdata.prodpasivems.model.dao.AccountDao;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface AccountRepository extends ReactiveMongoRepository<AccountDao,String> {
}
