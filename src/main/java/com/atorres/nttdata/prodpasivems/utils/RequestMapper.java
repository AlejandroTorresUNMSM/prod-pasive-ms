package com.atorres.nttdata.prodpasivems.utils;

import com.atorres.nttdata.prodpasivems.model.RequestAccount;
import com.atorres.nttdata.prodpasivems.model.creditms.CreditDao;
import com.atorres.nttdata.prodpasivems.model.creditms.RequestCredit;
import com.atorres.nttdata.prodpasivems.model.dao.AccountDao;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import org.springframework.stereotype.Component;

@Component
public class RequestMapper {
    public AccountDao accountToDao(RequestAccount requestAccount){
        return AccountDao.builder()
                .id(generateId())
                .type(requestAccount.getType())
                .balance(requestAccount.getBalance())
                .accountCategory(requestAccount.getAccountCategory())
                .build();
    }
    public AccountDto accountToDto(AccountDao accountDao){
        AccountDto accountDto= new AccountDto();
        accountDto.setId(accountDto.getId());
        accountDto.setType(accountDao.getType());
        accountDto.setAccountCategory(accountDao.getAccountCategory());
        accountDto.setBalance(accountDao.getBalance());
        accountDto.setClient(accountDao.getClient());
        return accountDto;
    }

    public CreditDao requestToDao(RequestCredit requestCredit){
        return CreditDao.builder()
                .id(generateId())
                .balance(requestCredit.getBalance())
                .debt(requestCredit.getBalance())
                .build();
    }

    private String generateId(){
        return java.util.UUID.randomUUID().toString().replace("-","");
    }
}